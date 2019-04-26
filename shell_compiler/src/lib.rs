#![allow(dead_code, unused_imports)]
use failure::{bail, err_msg, Fallible};
use shell_lexer::{Assignment, ParamExpr, ParamOper, WordComponent, WordComponentKind};
use shell_parser::{Command, CommandType, CompoundList, Redirection};
pub use shell_vm::*;
use std::collections::VecDeque;

mod registeralloc;
use registeralloc::RegisterAllocator;

#[derive(Default, Debug)]
struct FrameCompiler {
    allocator: RegisterAllocator,
    frame_start_program_address: usize,
}

impl std::ops::Deref for FrameCompiler {
    type Target = RegisterAllocator;
    fn deref(&self) -> &RegisterAllocator {
        &self.allocator
    }
}

impl std::ops::DerefMut for FrameCompiler {
    fn deref_mut(&mut self) -> &mut RegisterAllocator {
        &mut self.allocator
    }
}

#[derive(Default, Debug)]
pub struct Compiler {
    program: Vec<Operation>,
    frames: VecDeque<FrameCompiler>,
}

impl Compiler {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn finish(mut self) -> Fallible<Vec<Operation>> {
        self.push(op::Exit {
            value: Operand::LastWaitStatus,
        });
        Ok(self.program)
    }

    /// Emit a half-baked PushFrame instruction and set up a new
    /// register allocator block for the current context.
    /// commit_frame() must be called to fully bake the PushFrame
    /// instruction.
    fn reserve_frame(&mut self) {
        // Save the address of the PushFrame instruction so that we
        // can patch it up during commit_frame().
        let frame_start_program_address = self.program.len();
        // Use a dummy frame size; we'll patch this once we know the
        // full frame size.
        self.push(op::PushFrame { size: 0 });
        self.frames.push_back(FrameCompiler {
            allocator: RegisterAllocator::new(),
            frame_start_program_address,
        });
    }

    /// Called after registers have been allocated in a context and
    /// that context is no longer required.  Emits a PopFrame instruction
    /// to complement the PushFrame instruction emitted by reserve_frame().
    /// Patches up the PushFrame instruction that reserve_frame() emitted
    /// so that it reflects the size of this frame.
    fn commit_frame(&mut self) -> Fallible<()> {
        let frame = self
            .frames
            .pop_back()
            .ok_or_else(|| err_msg("no frame to commit"))?;
        self.program[frame.frame_start_program_address] = op::PushFrame {
            size: frame.frame_size(),
        }
        .into();
        self.push(op::PopFrame {});
        Ok(())
    }

    fn frame(&mut self) -> Fallible<&mut FrameCompiler> {
        self.frames.back_mut().ok_or_else(|| err_msg("no frame"))
    }

    /// Allocate a new empty list and return the frame relative
    /// slot associated with it.
    fn allocate_list(&mut self) -> Fallible<usize> {
        let slot = self.frame()?.allocate();
        self.push(op::Copy {
            source: Operand::Immediate(Value::List(vec![])),
            destination: Operand::FrameRelative(slot),
        });
        Ok(slot)
    }

    fn push<OP: Into<Operation>>(&mut self, op: OP) {
        self.program.push(op.into());
    }

    /// Allocate a new empty string and return the frame relative
    /// slot associated with it.
    fn allocate_string(&mut self) -> Fallible<usize> {
        let slot = self.frame()?.allocate();
        self.push(op::Copy {
            source: Operand::Immediate(Value::String(String::new())),
            destination: Operand::FrameRelative(slot),
        });
        Ok(slot)
    }

    /// For what is logically:
    /// `if (condition) { THEN } else { ELSE }`
    /// Emit a sequence like:
    ///
    /// ```norun
    ///   JumpIfZero .ELSE
    ///   {THEN}
    ///   Jump .DONE
    /// .ELSE
    ///   {ELSE}
    /// .DONE
    /// ```
    fn if_then_else<
        THEN: FnMut(&mut Compiler) -> Fallible<()>,
        ELSE: FnMut(&mut Compiler) -> Fallible<()>,
    >(
        &mut self,
        condition: Operand,
        mut then_: THEN,
        mut else_: ELSE,
    ) -> Fallible<()> {
        // Remember the opcode that we need to patch
        let first_jump = self.program.len();
        self.push(op::JumpIfZero {
            condition,
            target: InstructionAddress::Absolute(0),
        });

        then_(self)?;
        let second_jump = self.program.len();
        self.push(op::Jump {
            target: InstructionAddress::Absolute(0),
        });

        match self.program.get_mut(first_jump) {
            Some(Operation::JumpIfZero(op::JumpIfZero { ref mut target, .. })) => {
                *target = InstructionAddress::Absolute(second_jump + 1)
            }
            _ => bail!("opcode mismatch while patching jump"),
        };

        else_(self)?;

        let after = self.program.len();
        match self.program.get_mut(second_jump) {
            Some(Operation::Jump(op::Jump { ref mut target, .. })) => {
                *target = InstructionAddress::Absolute(after)
            }
            _ => bail!("opcode mismatch while patching jump"),
        };

        Ok(())
    }

    fn parameter_expand(&mut self, target_string: usize, expr: &ParamExpr) -> Fallible<()> {
        let slot = self.frame()?.allocate();
        self.push(op::GetEnv {
            name: Operand::Immediate(expr.name.as_str().into()),
            target: Operand::FrameRelative(slot),
        });
        match expr.kind {
            ParamOper::Get => self.push(op::Copy {
                source: Operand::FrameRelative(slot),
                destination: Operand::FrameRelative(target_string),
            }),
            ParamOper::GetDefault { allow_null } => {
                let test = self.frame()?.allocate();
                if allow_null {
                    self.push(op::IsNone {
                        source: Operand::FrameRelative(slot),
                        destination: Operand::FrameRelative(test),
                    });
                } else {
                    self.push(op::IsNoneOrEmptyString {
                        source: Operand::FrameRelative(slot),
                        destination: Operand::FrameRelative(test),
                    });
                }
                self.if_then_else(
                    Operand::FrameRelative(test),
                    |me| {
                        let argv = me.allocate_list()?;
                        for w in &expr.word {
                            me.word_expand(argv, w)?;
                        }
                        me.push(op::JoinList {
                            list: Operand::FrameRelative(argv),
                            destination: Operand::FrameRelative(target_string),
                        });
                        me.frame()?.free(argv);

                        Ok(())
                    },
                    |me| {
                        me.push(op::Copy {
                            source: Operand::FrameRelative(slot),
                            destination: Operand::FrameRelative(target_string),
                        });
                        Ok(())
                    },
                )?;
                self.frame()?.free(test);
            }
            _ => bail!("{:?} not implemented", expr),
        }
        Ok(())
    }

    /// Perform word expansion on word.
    /// Word is a list of components that are logically all part of the
    /// same field and thus are emitted into a string value together.
    /// However, some elements may be splittable which means that they
    /// are subject to field splitting based on the runtime value of
    /// the IFS variable.
    fn word_expand(&mut self, argv: usize, word: &Vec<WordComponent>) -> Fallible<()> {
        let expanded_word = self.allocate_string()?;

        // TODO: field splitting, backslashes
        let mut split = true;
        let glob = true;
        for component in word {
            if !component.splittable {
                split = false;
            }
            match &component.kind {
                WordComponentKind::Literal(literal) => {
                    self.push(op::StringAppend {
                        source: Operand::Immediate(Value::String(literal.to_owned())),
                        destination: Operand::FrameRelative(expanded_word),
                    });
                }
                WordComponentKind::TildeExpand(name) => {
                    let expanded = self.allocate_string()?;
                    self.push(op::TildeExpand {
                        name: Operand::Immediate(
                            name.as_ref().map(|s| s.into()).unwrap_or(Value::None),
                        ),
                        destination: Operand::FrameRelative(expanded),
                    });
                    self.push(op::StringAppend {
                        source: Operand::FrameRelative(expanded),
                        destination: Operand::FrameRelative(expanded_word),
                    });
                    self.frame()?.free(expanded);
                }
                WordComponentKind::ParamExpand(expr) => {
                    let expanded = self.allocate_string()?;
                    self.parameter_expand(expanded, expr)?;
                    self.push(op::StringAppend {
                        source: Operand::FrameRelative(expanded),
                        destination: Operand::FrameRelative(expanded_word),
                    });
                    self.frame()?.free(expanded);
                }
                WordComponentKind::CommandSubstitution(_) => bail!("command subst not implemented"),
            }
        }

        self.push(op::ListAppend {
            value: Operand::FrameRelative(expanded_word),
            list: Operand::FrameRelative(argv),
            split,
            glob,
        });

        self.frame()?.free(expanded_word);
        Ok(())
    }

    fn apply_redirection(&mut self, redir: &Vec<Redirection>) -> Fallible<bool> {
        if redir.is_empty() {
            return Ok(false);
        }
        self.push(op::PushIo {});

        for r in redir {
            match r {
                Redirection::File(f) => {
                    let filename = self.allocate_list()?;
                    self.word_expand(filename, &f.file_name)?;
                    self.push(op::OpenFile {
                        name: Operand::FrameRelative(filename),
                        fd_number: f.fd_number,
                        input: f.input,
                        output: f.output,
                        clobber: f.clobber,
                        append: f.append,
                    });
                    self.frame()?.free(filename);
                }
                Redirection::Fd(f) => {
                    self.push(op::DupFd {
                        src_fd: f.src_fd_number,
                        dest_fd: f.dest_fd_number,
                    });
                }
            }
        }

        Ok(true)
    }

    fn pop_redirection(&mut self, do_pop: bool) {
        if do_pop {
            self.push(op::PopIo {});
        }
    }

    fn process_assignments(&mut self, assignments: &Vec<Assignment>) -> Fallible<()> {
        for a in assignments {
            let value = self.allocate_list()?;
            self.word_expand(value, &a.value)?;
            self.push(op::JoinList {
                list: Operand::FrameRelative(value),
                destination: Operand::FrameRelative(value),
            });
            self.push(op::SetEnv {
                name: Operand::Immediate(a.name.as_str().into()),
                value: Operand::FrameRelative(value),
            });

            self.frame()?.free(value);
        }
        Ok(())
    }

    pub fn compile_command(&mut self, command: &Command) -> Fallible<()> {
        self.reserve_frame();
        let pop_outer_redir = self.apply_redirection(&command.redirects)?;

        match &command.command {
            CommandType::SimpleCommand(simple) => {
                // Goal: build up an argument list and then invoke it
                let argv = self.allocate_list()?;
                let pop_redir = self.apply_redirection(&simple.redirects)?;
                let pop_env = if !simple.words.is_empty() && !simple.assignments.is_empty() {
                    // Assignments are applicable only to the command we're
                    // setting up here, so push a new context.
                    self.push(op::PushEnvironment {});
                    true
                } else {
                    // Either there are no assignments, or they are supposed
                    // to last for longer than the current command invocation.
                    false
                };

                self.process_assignments(&simple.assignments)?;

                for word in &simple.words {
                    self.word_expand(argv, word)?;
                }

                let status = self.frame()?.allocate();
                self.push(op::SpawnCommand {
                    argv: Operand::FrameRelative(argv),
                    status: Operand::FrameRelative(status),
                });
                if !command.asynchronous {
                    self.push(op::Wait {
                        status: Operand::FrameRelative(status),
                    });
                }
                self.frame()?.free(status);

                if pop_env {
                    self.push(op::PopEnvironment {});
                }
                self.pop_redirection(pop_redir);
            }
            CommandType::If(cmd) => {
                // First evaluate the condition
                self.compound_list(&cmd.condition)?;
                self.if_then_else(
                    Operand::LastWaitStatus,
                    |me| {
                        if let Some(true_part) = &cmd.true_part {
                            me.compound_list(true_part)?;
                        }
                        Ok(())
                    },
                    |me| {
                        if let Some(false_part) = &cmd.false_part {
                            me.compound_list(false_part)?;
                        }
                        Ok(())
                    },
                )?;
            }
            _ => bail!("unhandled command type: {:?}", command),
        };

        self.pop_redirection(pop_outer_redir);
        self.commit_frame()?;
        Ok(())
    }

    fn compound_list(&mut self, list: &CompoundList) -> Fallible<()> {
        for command in &list.commands {
            self.compile_command(command)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use shell_parser::Parser;
    use std::ffi::{OsStr, OsString};
    use std::path::PathBuf;
    use std::sync::{Arc, Mutex};

    #[derive(Debug, Clone, PartialEq)]
    struct SpawnEntry {
        argv: Vec<Value>,
        environment: Environment,
        current_directory: PathBuf,
    }

    impl SpawnEntry {
        fn new(argv: Vec<Value>) -> Self {
            let environment = Environment::new_empty();
            let current_directory = std::env::current_dir().unwrap();
            Self {
                argv,
                environment,
                current_directory,
            }
        }

        fn set_env(mut self, key: &str, value: &str) -> Self {
            self.environment.set(key, value);
            self
        }
    }

    #[derive(Default, Debug)]
    struct TestHost {
        spawn_log: Arc<Mutex<Vec<SpawnEntry>>>,
    }

    impl ShellHost for TestHost {
        fn lookup_homedir(&self, user: Option<&str>) -> Fallible<OsString> {
            Ok(match user {
                Some("wez") => "/home/wez".into(),
                None => "/home/wez".into(),
                Some("one") => "/home/one".into(),
                Some(name) => bail!("unknown user {:?}", name),
            })
        }

        fn spawn_command(
            &self,
            argv: &Vec<Value>,
            environment: &mut Environment,
            current_directory: &mut PathBuf,
            _io_env: &IoEnvironment,
        ) -> Fallible<WaitableStatus> {
            let mut log = self.spawn_log.lock().unwrap();

            let command = argv
                .get(0)
                .ok_or_else(|| err_msg("argv0 is missing"))?
                .as_os_str()
                .ok_or_else(|| err_msg("argv0 is not a string"))?;
            let status = if command == "true" || command == "echo" {
                0
            } else if command == "false" {
                // false is explicitly non-zero
                1
            } else {
                // Anything else we don't recognize is an error
                2
            };

            log.push(SpawnEntry {
                argv: argv.clone(),
                environment: environment.clone(),
                current_directory: current_directory.clone(),
            });
            Ok(Status::Complete(status.into()).into())
        }
    }

    fn compile(prog: &str) -> Fallible<Vec<Operation>> {
        let mut parser = Parser::new(prog.as_bytes());
        let command = parser.parse()?;
        let mut compiler = Compiler::new();
        compiler.compile_command(&command)?;
        compiler.finish()
    }

    fn run(prog: Vec<Operation>) -> Fallible<Status> {
        let mut machine = Machine::new(&Program::new(prog), Some(Environment::new_empty()))?;
        machine.run()
    }

    fn print_prog(prog: &Vec<Operation>) {
        eprintln!("--");
        eprintln!("program of length {}", prog.len());
        for (idx, op) in prog.iter().enumerate() {
            eprintln!("[{:3}]   {:?}", idx, op);
        }
        eprintln!("--");
    }

    fn run_with_log(prog: Vec<Operation>) -> Fallible<(Status, Vec<SpawnEntry>)> {
        print_prog(&prog);
        let mut machine = Machine::new(&Program::new(prog), Some(Environment::new_empty()))?;

        let host = TestHost::default();
        let log = Arc::clone(&host.spawn_log);
        machine.set_host(Arc::new(host));
        let status = machine.run()?;
        let log = {
            let locked = log.lock().unwrap();
            locked.clone()
        };
        Ok((status, log))
    }

    #[test]
    fn basic_echo() -> Fallible<()> {
        let ops = compile("echo hello")?;
        assert_eq!(
            ops,
            vec![
                Operation::PushFrame(op::PushFrame { size: 2 }),
                Operation::Copy(op::Copy {
                    source: Operand::Immediate(Value::List(vec![])),
                    destination: Operand::FrameRelative(1)
                }),
                Operation::Copy(op::Copy {
                    source: Operand::Immediate("".into()),
                    destination: Operand::FrameRelative(2)
                }),
                Operation::StringAppend(op::StringAppend {
                    source: Operand::Immediate("echo".into()),
                    destination: Operand::FrameRelative(2)
                }),
                Operation::ListAppend(op::ListAppend {
                    value: Operand::FrameRelative(2),
                    list: Operand::FrameRelative(1),
                    split: true,
                    glob: true,
                }),
                Operation::Copy(op::Copy {
                    source: Operand::Immediate("".into()),
                    destination: Operand::FrameRelative(2)
                }),
                Operation::StringAppend(op::StringAppend {
                    source: Operand::Immediate("hello".into()),
                    destination: Operand::FrameRelative(2)
                }),
                Operation::ListAppend(op::ListAppend {
                    value: Operand::FrameRelative(2),
                    list: Operand::FrameRelative(1),
                    split: true,
                    glob: true,
                }),
                op::SpawnCommand {
                    argv: Operand::FrameRelative(1),
                    status: Operand::FrameRelative(2),
                }
                .into(),
                op::Wait {
                    status: Operand::FrameRelative(2),
                }
                .into(),
                op::PopFrame {}.into(),
                op::Exit {
                    value: Operand::LastWaitStatus
                }
                .into(),
            ]
        );
        assert_eq!(
            run_with_log(ops)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "hello".into()],)]
            )
        );
        Ok(())
    }

    #[test]
    fn test_conditional_pipe() -> Fallible<()> {
        assert_eq!(
            run_with_log(compile("true || false")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["true".into()]),]
            )
        );

        assert_eq!(
            run_with_log(compile("true && false")?)?,
            (
                Status::Complete(1.into()),
                vec![
                    SpawnEntry::new(vec!["true".into()],),
                    SpawnEntry::new(vec!["false".into()],),
                ]
            )
        );

        assert_eq!(
            run_with_log(compile("false || true")?)?,
            (
                Status::Complete(0.into()),
                vec![
                    SpawnEntry::new(vec!["false".into()],),
                    SpawnEntry::new(vec!["true".into()],),
                ]
            )
        );
        Ok(())
    }

    #[test]
    fn test_param_get() -> Fallible<()> {
        assert_eq!(
            run_with_log(compile("echo $foo")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into()]),]
            )
        );

        assert_eq!(
            run_with_log(compile("echo \"$foo\"")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "".into()]),]
            )
        );

        assert_eq!(
            run_with_log(compile("foo=1 echo $foo")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "1".into()]).set_env("foo", "1"),]
            )
        );

        Ok(())
    }

    #[test]
    fn test_param_get_default() -> Fallible<()> {
        assert_eq!(
            run_with_log(compile("echo ${foo:-bar}")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "bar".into()]),]
            )
        );

        assert_eq!(
            run_with_log(compile("echo ${foo:-bar baz}")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec![
                    "echo".into(),
                    "bar".into(),
                    "baz".into()
                ]),]
            )
        );

        assert_eq!(
            run_with_log(compile("echo \"${foo:-bar baz}\"")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "bar baz".into()]),]
            )
        );

        assert_eq!(
            run_with_log(compile("foo=1 echo ${foo:-bar}")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "1".into()]).set_env("foo", "1"),]
            )
        );

        assert_eq!(
            run_with_log(compile("foo='' echo ${foo:-bar}")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into(), "bar".into()]).set_env("foo", ""),]
            )
        );

        assert_eq!(
            run_with_log(compile("foo= echo ${foo-bar}")?)?,
            (
                Status::Complete(0.into()),
                vec![SpawnEntry::new(vec!["echo".into()]).set_env("foo", ""),]
            )
        );

        Ok(())
    }
}
