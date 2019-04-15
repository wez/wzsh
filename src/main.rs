use failure::{bail, ensure, format_err, Error, Fail, Fallible};
use std::cell::RefCell;
use std::process::Command;
use std::rc::Rc;

use shlex::error::{LexError, TokenPosition};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, Token, TokenKind};

mod exitstatus;
mod filedescriptor;
mod parse;
mod repl;

use exitstatus::{ExitStatus, WaitableExitStatus};
use filedescriptor::FileDescriptor;
use parse::{
    Command as ShellCommand, CommandType, FileRedirection, ParseErrorKind, Redirection,
    SimpleCommand,
};
use repl::repl;

pub struct ShellExpander {}
impl Expander for ShellExpander {
    fn lookup_homedir(
        &self,
        user: Option<&str>,
        environment: &mut Environment,
    ) -> Fallible<ShellString> {
        if user.is_none() {
            let home = environment
                .get("HOME")
                .ok_or_else(|| format_err!("HOME is not set"))?;
            Ok(home.as_os_str().into())
        } else {
            bail!("ze goggles");
        }
    }
}

#[derive(Clone)]
pub struct ExecutionEnvironment {
    env: Rc<RefCell<Environment>>,
    aliases: Rc<RefCell<Aliases>>,

    stdin: Rc<RefCell<FileDescriptor>>,
    stdout: Rc<RefCell<FileDescriptor>>,
    stderr: Rc<RefCell<FileDescriptor>>,
}

impl ExecutionEnvironment {
    pub fn new() -> Fallible<Self> {
        Ok(Self {
            env: Rc::new(RefCell::new(Environment::new())),
            aliases: Rc::new(RefCell::new(Aliases::new())),
            stdin: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stdin())?)),
            stdout: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stdout())?)),
            stderr: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stderr())?)),
        })
    }

    pub fn set_stdin(&mut self, fd: FileDescriptor) {
        self.stdin = Rc::new(RefCell::new(fd));
    }

    pub fn set_stdout(&mut self, fd: FileDescriptor) {
        self.stdout = Rc::new(RefCell::new(fd));
    }

    pub fn build_command(
        &mut self,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
    ) -> Fallible<Option<std::process::Command>> {
        let argv = cmd.expand_argv(&mut self.env.borrow_mut(), expander, &self.aliases.borrow())?;
        if argv.is_empty() {
            Ok(None)
        } else {
            let mut child_cmd = Command::new(&argv[0]);
            for arg in argv.iter().skip(1) {
                child_cmd.arg(arg);
            }
            child_cmd.env_clear();
            child_cmd.envs(self.env.borrow().iter());

            child_cmd.stdin(self.stdin.borrow_mut().as_stdio()?);
            child_cmd.stdout(self.stdout.borrow_mut().as_stdio()?);
            child_cmd.stderr(self.stderr.borrow_mut().as_stdio()?);
            Ok(Some(child_cmd))
        }
    }

    fn file_from_redir(
        &mut self,
        redir: &FileRedirection,
        expander: &ShellExpander,
    ) -> Fallible<std::fs::File> {
        ensure!(
            redir.fd_number < 3,
            "only stdin, stdout, stderr currently support for redirection"
        );
        let file_name = match &redir.file_name.kind {
            TokenKind::Word(word) => {
                let word = ShellString::from(word.as_str());
                let file = expander.expand_word(&word, &mut self.env.borrow_mut())?;
                ensure!(
                    file.len() == 1,
                    "{:?} expanded to {:?}, expected just a single item",
                    redir.file_name,
                    file
                );
                file.into_iter().next().unwrap()
            }
            _ => bail!("file_name is not a word token"),
        };
        let mut options = std::fs::OpenOptions::new();
        options
            .read(redir.input)
            .write(redir.output)
            .append(redir.append)
            .truncate(redir.output && !redir.append)
            // TODO: if a noclobber option is set, and !redir.clobber,
            // then we should look at .create_new() instead
            .create(redir.output || redir.clobber);
        match options.open(file_name.as_ref()) {
            Ok(file) => Ok(file),
            Err(e) => {
                return Err(e
                    .context(format!("opening '{}' using {:#?}", file_name, options))
                    .into());
            }
        }
    }

    pub fn apply_redirections_to_env(
        &mut self,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
    ) -> Fallible<()> {
        for redir in &cmd.redirections {
            match redir {
                Redirection::File(redir) => {
                    let file = self.file_from_redir(redir, expander)?;
                    let fd = Rc::new(RefCell::new(FileDescriptor::dup(file)?));
                    match redir.fd_number {
                        0 => self.stdin = fd,
                        1 => self.stdout = fd,
                        2 => self.stderr = fd,
                        _ => bail!("unsupported fd number"),
                    };
                }
                Redirection::Fd(_) => {
                    bail!("fd redirection not hooked up to eval");
                }
            }
        }
        Ok(())
    }

    pub fn apply_redirections_to_cmd(
        &mut self,
        child_cmd: &mut std::process::Command,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
    ) -> Fallible<()> {
        for redir in &cmd.redirections {
            match redir {
                Redirection::File(redir) => {
                    let file = self.file_from_redir(redir, expander)?;
                    match redir.fd_number {
                        0 => child_cmd.stdin(file),
                        1 => child_cmd.stdout(file),
                        2 => child_cmd.stderr(file),
                        _ => bail!("unsupported fd number"),
                    };
                }
                Redirection::Fd(_) => {
                    bail!("fd redirection not hooked up to eval");
                }
            }
        }
        Ok(())
    }

    fn apply_assignments_to_cmd(
        &mut self,
        expander: &ShellExpander,
        assignments: &Vec<Token>,
        child_cmd: &mut std::process::Command,
    ) -> Fallible<()> {
        for t in assignments {
            if let Some((key, value)) = t.kind.parse_assignment_word() {
                let fields = expander.expand_word(&value.into(), &mut self.env.borrow_mut())?;
                ensure!(
                    fields.len() == 1,
                    "variable expansion produced {:?}, expected a single field"
                );
                child_cmd.env(key, &fields[0]);
            }
        }
        Ok(())
    }
    fn apply_assignments_to_env(
        &mut self,
        expander: &ShellExpander,
        assignments: &Vec<Token>,
    ) -> Fallible<()> {
        for t in assignments {
            if let Some((key, value)) = t.kind.parse_assignment_word() {
                let fields = expander.expand_word(&value.into(), &mut self.env.borrow_mut())?;
                ensure!(
                    fields.len() == 1,
                    "variable expansion produced {:?}, expected a single field"
                );
                self.env.borrow_mut().set(key, &fields[0]);
            }
        }
        Ok(())
    }

    pub fn eval(
        &mut self,
        expander: &ShellExpander,
        command: &ShellCommand,
    ) -> Fallible<WaitableExitStatus> {
        match &command.command {
            CommandType::SimpleCommand(cmd) => {
                if let Some(mut child_cmd) = self.build_command(&cmd, expander)? {
                    self.apply_redirections_to_cmd(&mut child_cmd, &cmd, expander)?;
                    self.apply_assignments_to_cmd(expander, &cmd.assignments, &mut child_cmd)?;
                    let child = child_cmd.spawn()?;
                    WaitableExitStatus::with_child(command.asynchronous, child)
                } else {
                    self.apply_redirections_to_env(&cmd, expander)?;
                    self.apply_assignments_to_env(expander, &cmd.assignments)?;
                    Ok(WaitableExitStatus::Done(ExitStatus::new_ok()))
                }
            }
            CommandType::BraceGroup(list) => {
                let mut status = WaitableExitStatus::Done(ExitStatus::new_ok());
                for cmd in &list.commands {
                    status = self.eval(expander, &cmd)?;
                }
                Ok(status)
            }
            CommandType::Subshell(list) => {
                // Posix wants the subshell to be a forked child,
                // but we don't do that in the interests of portability
                // to windows.  Instead we clone the current execution
                // environment and use that to run the list.
                // We'll probably need to do something smarter
                // here to manage signals/process groups on posix systems,
                // and for async subshells, may want to consider using
                // a thread to execute it.
                let mut sub_env = self.clone();
                let mut status = WaitableExitStatus::Done(ExitStatus::new_ok());
                for cmd in &list.commands {
                    status = sub_env.eval(expander, &cmd)?;
                }
                Ok(status)
            }
            CommandType::Pipeline(pipeline) => {
                let mut envs = vec![];
                for _ in 0..pipeline.commands.len() {
                    envs.push(self.clone());
                }

                for i in 0..pipeline.commands.len() - 1 {
                    let pipe = FileDescriptor::pipe()?;

                    envs[i].set_stdout(pipe.write);
                    envs[i + 1].set_stdin(pipe.read);
                }

                // We need to release the pipe handles as we
                // work through the pipe sequence, and the easiest
                // way to do that is to reverse this sequence and
                // pop the envs off as we walk forwards through the
                // pipeline.
                envs.reverse();

                let mut status = WaitableExitStatus::Done(ExitStatus::new_ok());
                for cmd in &pipeline.commands {
                    let mut env = envs.pop().unwrap();
                    status = env.eval(expander, cmd)?;
                    // We want to drop the env so that its ref to the
                    // pipes is released, causing the pipes to cascade
                    // shut as we move along.
                    drop(env);
                }

                let status = status.wait()?;
                let status = if pipeline.inverted {
                    status.invert()
                } else {
                    status
                };
                Ok(WaitableExitStatus::Done(status))
            }
            _ => bail!("eval doesn't know about {:#?}", command),
        }
    }
}

fn extract_error_range(e: &Error) -> Option<(TokenPosition, TokenPosition)> {
    if let Some(lex_err) = e.downcast_ref::<LexError>() {
        Some((lex_err.start, lex_err.end))
    } else if let Some(parse_err) = e.downcast_ref::<ParseErrorKind>() {
        match parse_err {
            ParseErrorKind::UnexpectedToken(token, ..) => Some((token.start, token.end)),
        }
    } else {
        None
    }
}

fn print_error(e: &Error, input: &str) {
    for item in e.iter_chain() {
        eprintln!("wzsh: {}", item);
    }
    if let Some((start, end)) = extract_error_range(e) {
        let lines: Vec<&str> = input.split('\n').collect();

        let start_line = &lines[start.line];
        let end_line = &lines[end.line];

        let mut indicator = String::new();
        let end_col = if start.line == end.line {
            end.col
        } else {
            start_line.len()
        };

        for _ in 0..start.col {
            indicator.push(' ');
        }

        indicator.push_str("\x1b[1m");
        for _ in start.col..=end_col {
            indicator.push('^');
        }
        indicator.push_str("\x1b[0m");

        eprintln!("{}", start_line);
        eprintln!("{}", indicator);

        if end.line != start.line {
            indicator.clear();
            indicator.push_str("\x1b[1m");
            for _ in 0..=end.col {
                indicator.push('^');
            }
            indicator.push_str("\x1b[0m");
            eprintln!("{}", end_line);
            eprintln!("{}", indicator);
        }
    }
}

fn main() -> Result<(), Error> {
    let env = ExecutionEnvironment::new()?;
    let expander = ShellExpander {};
    repl(env, expander)
}
