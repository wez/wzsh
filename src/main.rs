use failure::{bail, ensure, format_err, Error, Fail, Fallible};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{Config, Editor, Helper};
use std::borrow::Cow;
use std::cell::RefCell;
use std::os::unix::prelude::*;
use std::process::Command;
use std::rc::Rc;

use shlex::error::{LexError, TokenPosition};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, LexErrorKind, Token, TokenKind};
mod parse;
use parse::{
    Command as ShellCommand, CommandType, FileRedirection, ParseErrorKind, Parser, Redirection,
    SimpleCommand,
};

struct ShellExpander {}
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

struct LineEditorHelper {
    completer: FilenameCompleter,
}

impl Completer for LineEditorHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize) -> Result<(usize, Vec<Pair>), ReadlineError> {
        self.completer.complete(line, pos)
    }
}

impl Hinter for LineEditorHelper {
    fn hint(&self, line: &str, _pos: usize) -> Option<String> {
        if line == "ls" {
            Some(" -l".to_owned())
        } else {
            None
        }
    }
}

impl Highlighter for LineEditorHelper {
    fn highlight_prompt<'p>(&self, prompt: &'p str) -> Cow<'p, str> {
        Cow::Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        Cow::Borrowed(line)
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        true
    }
}

impl Helper for LineEditorHelper {}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct ExitStatus {
    code: i32,
}

impl ExitStatus {
    pub fn new_ok() -> Self {
        Self { code: 0 }
    }
    pub fn new_fail() -> Self {
        Self { code: 1 }
    }

    pub fn is_ok(&self) -> bool {
        self.code == 0
    }

    pub fn invert(&self) -> ExitStatus {
        if self.is_ok() {
            ExitStatus { code: 1 }
        } else {
            ExitStatus { code: 0 }
        }
    }
}

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(s: std::process::ExitStatus) -> ExitStatus {
        if let Some(code) = s.code() {
            ExitStatus { code }
        } else {
            // FIXME: do something smarter about reporting signals
            ExitStatus { code: 127 }
        }
    }
}

struct FileDescriptor {
    fd: RawFd,
}

impl Drop for FileDescriptor {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.fd);
        }
    }
}

impl AsRawFd for FileDescriptor {
    fn as_raw_fd(&self) -> RawFd {
        self.fd
    }
}

fn dup(fd: RawFd) -> Fallible<FileDescriptor> {
    let duped = unsafe { libc::dup(fd) };
    if duped == -1 {
        bail!(
            "dup of fd {} failed: {:?}",
            fd,
            std::io::Error::last_os_error()
        )
    } else {
        let mut owned = FileDescriptor { fd: duped };
        owned.cloexec()?;
        Ok(owned)
    }
}

struct Pipes {
    read: FileDescriptor,
    write: FileDescriptor,
}

impl FileDescriptor {
    pub fn dup<F: AsRawFd>(f: F) -> Fallible<Self> {
        dup(f.as_raw_fd())
    }

    pub fn pipe() -> Fallible<Pipes> {
        let mut fds = [-1i32; 2];
        let res = unsafe { libc::pipe(fds.as_mut_ptr()) };
        if res == -1 {
            bail!(
                "failed to create a pipe: {:?}",
                std::io::Error::last_os_error()
            )
        } else {
            let mut read = FileDescriptor { fd: fds[0] };
            let mut write = FileDescriptor { fd: fds[1] };
            read.cloexec()?;
            write.cloexec()?;
            Ok(Pipes { read, write })
        }
    }

    /// Helper function to set the close-on-exec flag for a raw descriptor
    fn cloexec(&mut self) -> Fallible<()> {
        let flags = unsafe { libc::fcntl(self.fd, libc::F_GETFD) };
        if flags == -1 {
            bail!(
                "fcntl to read flags failed: {:?}",
                std::io::Error::last_os_error()
            );
        }
        let result = unsafe { libc::fcntl(self.fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) };
        if result == -1 {
            bail!(
                "fcntl to set CLOEXEC failed: {:?}",
                std::io::Error::last_os_error()
            );
        }
        Ok(())
    }

    fn as_stdio(&self) -> Fallible<std::process::Stdio> {
        let duped = dup(self.fd)?;
        let fd = duped.fd;
        let stdio = unsafe { std::process::Stdio::from_raw_fd(fd) };
        std::mem::forget(duped); // don't drop; stdio now owns it
        Ok(stdio)
    }
}

#[derive(Clone)]
struct ExecutionEnvironment {
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
    ) -> Fallible<ExitStatus> {
        match &command.command {
            CommandType::SimpleCommand(cmd) => {
                if let Some(mut child_cmd) = self.build_command(&cmd, expander)? {
                    self.apply_redirections_to_cmd(&mut child_cmd, &cmd, expander)?;
                    self.apply_assignments_to_cmd(expander, &cmd.assignments, &mut child_cmd)?;
                    let mut child = child_cmd.spawn()?;
                    Ok(child.wait()?.into())
                } else {
                    self.apply_redirections_to_env(&cmd, expander)?;
                    self.apply_assignments_to_env(expander, &cmd.assignments)?;
                    Ok(ExitStatus::new_ok())
                }
            }
            CommandType::BraceGroup(list) => {
                let mut status = ExitStatus::new_ok();
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
                // here to manage signals/process groups on posix systems.
                let mut sub_env = self.clone();
                let mut status = ExitStatus::new_ok();
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

                let mut status = ExitStatus::new_ok();
                for cmd in &pipeline.commands {
                    let mut env = envs.pop().unwrap();
                    status = env.eval(expander, cmd)?;
                    // We want to drop the env so that its ref to the
                    // pipes is released, causing the pipes to cascade
                    // shut as we move along.
                    // TODO: we should make each of the pipe commands
                    // (except the last) async and not wait for them.
                    drop(env);
                }

                if pipeline.inverted {
                    Ok(status.invert())
                } else {
                    Ok(status)
                }
            }
            _ => bail!("eval doesn't know about {:#?}", command),
        }
    }
}

/// Returns true if a given error might be resolved by allowing
/// the user to continue typing more text on a subsequent line.
/// Most lex errors fall into that category.
fn is_recoverable_parse_error(e: &Error) -> bool {
    if let Some(lex_err) = e.downcast_ref::<LexError>() {
        match lex_err.kind {
            LexErrorKind::EofDuringBackslash
            | LexErrorKind::EofDuringComment
            | LexErrorKind::EofDuringSingleQuotedString
            | LexErrorKind::EofDuringDoubleQuotedString
            | LexErrorKind::EofDuringParameterExpansion => true,
            LexErrorKind::IoError => false,
        }
    } else if let Some(parse_err) = e.downcast_ref::<ParseErrorKind>() {
        match parse_err {
            ParseErrorKind::UnexpectedToken(..) => false,
        }
    } else {
        false
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
    let config = Config::builder().history_ignore_space(true).build();

    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(LineEditorHelper {
        completer: FilenameCompleter::new(),
    }));
    rl.load_history("history.txt").ok();

    let mut env = ExecutionEnvironment::new()?;
    let expander = ShellExpander {};

    let mut input = String::new();
    let mut last_status = ExitStatus::new_ok();

    loop {
        let prompt = match (input.is_empty(), last_status) {
            (true, st) if !st.is_ok() => format!("[{}] $ ", st.code),
            (true, _) => "$ ".to_owned(),
            (false, _) => "..> ".to_owned(),
        };

        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());

                input.push_str(&line);

                let mut parser = Parser::new("stdin", input.as_bytes());
                let list = match parser.parse() {
                    Err(e) => {
                        if !is_recoverable_parse_error(&e) {
                            print_error(&e, &input);
                            input.clear();
                            last_status = ExitStatus::new_fail();
                        } else {
                            input.push('\n');
                        }
                        continue;
                    }
                    Ok(list) => {
                        input.clear();
                        list
                    }
                };
                last_status = match env.eval(&expander, &list) {
                    Err(e) => {
                        print_error(&e, &input);
                        ExitStatus::new_fail()
                    }
                    Ok(status) => status,
                };
            }
            Err(ReadlineError::Interrupted) => {
                input.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }

    Ok(())
}
