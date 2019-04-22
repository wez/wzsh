#![allow(dead_code, unused_imports)]
use failure::{bail, err_msg, Fallible};
use shell_lexer::{WordComponent, WordComponentKind};
use shell_parser::{Command, CommandType};
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

    pub fn finish(self) -> Vec<Operation> {
        self.program
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
        self.program.push(Operation::PushFrame { size: 0 });
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
        self.program[frame.frame_start_program_address] = Operation::PushFrame {
            size: frame.frame_size(),
        };
        self.program.push(Operation::PopFrame);
        Ok(())
    }

    fn frame(&mut self) -> Fallible<&mut FrameCompiler> {
        self.frames.back_mut().ok_or_else(|| err_msg("no frame"))
    }

    /// Allocate a new empty list and return the frame relative
    /// slot associated with it.
    fn allocate_list(&mut self) -> Fallible<usize> {
        let slot = self.frame()?.allocate();
        self.program.push(Operation::Copy {
            source: Operand::Immediate(Value::List(vec![])),
            destination: Operand::FrameRelative(slot),
        });
        Ok(slot)
    }

    /// Allocate a new empty string and return the frame relative
    /// slot associated with it.
    fn allocate_string(&mut self) -> Fallible<usize> {
        let slot = self.frame()?.allocate();
        self.program.push(Operation::Copy {
            source: Operand::Immediate(Value::String(String::new())),
            destination: Operand::FrameRelative(slot),
        });
        Ok(slot)
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

        for component in word {
            match &component.kind {
                WordComponentKind::Literal(literal) => {
                    self.program.push(Operation::StringAppend {
                        source: Operand::Immediate(Value::String(literal.to_owned())),
                        destination: Operand::FrameRelative(expanded_word),
                    });
                }
                _ => bail!("unhandled word component in word_expand: {:?}", component),
            }
        }

        self.program.push(Operation::ListAppend {
            value: Operand::FrameRelative(expanded_word),
            list: Operand::FrameRelative(argv),
        });

        self.frame()?.free(expanded_word);
        Ok(())
    }

    pub fn compile_command(&mut self, command: &Command) -> Fallible<()> {
        self.reserve_frame();
        match &command.command {
            CommandType::SimpleCommand(simple) => {
                // Goal: build up an argument list and then invoke it
                let argv = self.allocate_list()?;
                for word in &simple.words {
                    self.word_expand(argv, word)?;
                }
                self.program.push(Operation::Exit {
                    value: Operand::FrameRelative(argv),
                });
            }
            _ => bail!("unhandled command type: {:?}", command),
        };
        self.commit_frame()?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use shell_parser::Parser;
    use std::sync::Arc;

    fn compile(prog: &str) -> Fallible<Vec<Operation>> {
        let mut parser = Parser::new(prog.as_bytes());
        let command = parser.parse()?;
        let mut compiler = Compiler::new();
        compiler.compile_command(&command)?;
        Ok(compiler.finish())
    }

    fn run(prog: Vec<Operation>) -> Fallible<Status> {
        let mut machine = Machine::new(&Program::new(prog));
        machine.run()
    }

    #[test]
    fn basic_echo() -> Fallible<()> {
        let ops = compile("echo hello")?;
        assert_eq!(
            ops,
            vec![
                Operation::PushFrame { size: 2 },
                Operation::Copy {
                    source: Operand::Immediate(Value::List(vec![])),
                    destination: Operand::FrameRelative(1)
                },
                Operation::Copy {
                    source: Operand::Immediate("".into()),
                    destination: Operand::FrameRelative(2)
                },
                Operation::StringAppend {
                    source: Operand::Immediate("echo".into()),
                    destination: Operand::FrameRelative(2)
                },
                Operation::ListAppend {
                    value: Operand::FrameRelative(2),
                    list: Operand::FrameRelative(1)
                },
                Operation::Copy {
                    source: Operand::Immediate("".into()),
                    destination: Operand::FrameRelative(2)
                },
                Operation::StringAppend {
                    source: Operand::Immediate("hello".into()),
                    destination: Operand::FrameRelative(2)
                },
                Operation::ListAppend {
                    value: Operand::FrameRelative(2),
                    list: Operand::FrameRelative(1)
                },
                Operation::Exit {
                    value: Operand::FrameRelative(1)
                },
                Operation::PopFrame
            ]
        );
        assert_eq!(
            run(ops)?,
            Status::Complete(Value::List(vec!["echo".into(), "hello".into()]))
        );
        Ok(())
    }

}
