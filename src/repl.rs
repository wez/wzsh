use crate::errorprint::print_error;
use crate::exitstatus::UnixChild;
use crate::job::{
    add_to_process_group, make_foreground_process_group, put_shell_in_foreground, Job, JobList,
};
use crate::pathsearch::PathSearcher;
use failure::{bail, err_msg, format_err, Error, Fail, Fallible};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{Config, Editor, Helper};
use shell_compiler::Compiler;
use shell_lexer::{LexError, LexErrorKind};
use shell_parser::{ParseErrorKind, Parser};
use shell_vm::{
    Environment, IoEnvironment, Machine, Program, ShellHost, Status, Value, WaitableStatus,
};
use std::borrow::Cow;
use std::ffi::OsString;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

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
            | LexErrorKind::EofDuringAssignmentWord
            | LexErrorKind::EofDuringCommandSubstitution
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

fn init_job_control() -> Fallible<()> {
    let pty_fd = 0;
    unsafe {
        // Loop until we are in the foreground.
        loop {
            let pgrp = libc::tcgetpgrp(pty_fd);
            let shell_pgid = libc::getpgrp();
            if shell_pgid == pgrp {
                break;
            }
            libc::kill(-shell_pgid, libc::SIGTTIN);
        }

        // Ignore interactive and job control signals
        for s in &[
            libc::SIGINT,
            libc::SIGQUIT,
            libc::SIGTSTP,
            libc::SIGTTIN,
            libc::SIGTTOU,
            // libc::SIGCHLD : we need to leave SIGCHLD alone,
            // otherwise waitpid returns ECHILD
        ] {
            libc::signal(*s, libc::SIG_IGN);
        }

        // Put ourselves in our own process group
        let shell_pgid = libc::getpid();
        if libc::setpgid(shell_pgid, shell_pgid) != 0 {
            return Err(std::io::Error::last_os_error()
                .context("unable to put shell into its own process group")
                .into());
        }

        // Grab control of the terminal
        libc::tcsetpgrp(pty_fd, shell_pgid);

        // TODO: tcgetattr to save terminal attributes
    }
    Ok(())
}

#[derive(Debug)]
struct Host {
    job: Mutex<Job>,
}

impl ShellHost for Host {
    fn lookup_homedir(&self, user: Option<&str>) -> Fallible<OsString> {
        bail!("lookup_homedir not implemented");
    }
    fn spawn_command(
        &self,
        argv: &Vec<Value>,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus> {
        if argv.is_empty() {
            return Ok(Status::Complete(0.into()).into());
        }

        let (search_builtin, search_path, argv) = if argv[0].as_str() == Some("command") {
            (false, true, &argv[1..])
        } else if argv[0].as_str() == Some("builtin") {
            (true, false, &argv[1..])
        } else {
            (true, true, &argv[..])
        };

        if search_path {
            if let Some(exe) = PathSearcher::new(
                argv[0]
                    .as_os_str()
                    .ok_or_else(|| err_msg("argv0 is not convertible to OsStr"))?,
                environment,
            )
            .next()
            {
                let mut child_cmd = std::process::Command::new(&exe);
                for (i, arg) in argv.iter().enumerate().skip(1) {
                    child_cmd.arg(
                        arg.as_os_str()
                            .ok_or_else(|| format_err!("argv{} is not convertible to OsStr", i))?,
                    );
                }
                child_cmd.env_clear();
                child_cmd.envs(environment.iter());
                child_cmd.current_dir(&current_directory);

                child_cmd.stdin(io_env.fd_as_stdio(0)?);
                child_cmd.stdout(io_env.fd_as_stdio(1)?);
                child_cmd.stderr(io_env.fd_as_stdio(2)?);

                let process_group_id = self.job.lock().unwrap().process_group_id();

                #[cfg(unix)]
                unsafe {
                    use std::os::unix::process::CommandExt;
                    child_cmd.pre_exec(move || {
                        let pid = libc::getpid();
                        if process_group_id == 0 {
                            /*
                            if asynchronous {
                                make_own_process_group(pid);
                            } else {
                            */
                            make_foreground_process_group(pid);
                        } else {
                            add_to_process_group(pid, process_group_id);
                        }
                        for s in &[
                            libc::SIGINT,
                            libc::SIGQUIT,
                            libc::SIGTSTP,
                            libc::SIGTTIN,
                            libc::SIGTTOU,
                            libc::SIGCHLD,
                        ] {
                            libc::signal(*s, libc::SIG_DFL);
                        }

                        Ok(())
                    });
                }

                let child = child_cmd.spawn()?;

                let child = UnixChild::new(child);

                // To avoid a race condition with starting up the child, we
                // need to also munge the process group assignment here in
                // the parent.  Note that the loser of the race will experience
                // errors in attempting this block, so we willfully ignore
                // the return values here: we cannot do anything about them.
                #[cfg(unix)]
                {
                    let pid = child.pid();
                    if process_group_id == 0 {
                        /*
                        if asynchronous {
                            make_own_process_group(pid);
                        } else {
                        */
                        make_foreground_process_group(pid);
                    } else {
                        add_to_process_group(pid, process_group_id);
                    }
                }

                self.job.lock().unwrap().add(child.clone())?;

                return Ok(WaitableStatus::new(Arc::new(child)));
            }
        }

        if let Some(s) = argv[0].as_str() {
            writeln!(io_env.stderr(), "wzsh: {} not found", s)?;
        } else {
            writeln!(io_env.stderr(), "wzsh: {:?} not found", &argv[0])?;
        }
        Ok(Status::Complete(1.into()).into())
    }
}

struct EnvBits {
    cwd: PathBuf,
    env: Environment,
    jobs: JobList,
}

fn compile_and_run(prog: &str, env_bits: &mut EnvBits) -> Fallible<Status> {
    let job = Mutex::new(Job::new_empty(prog.to_owned()));
    let mut parser = Parser::new(prog.as_bytes());
    let command = parser.parse()?;
    let mut compiler = Compiler::new();
    compiler.compile_command(&command)?;
    let prog = compiler.finish()?;
    let mut machine = Machine::new(&Program::new(prog), Some(env_bits.env.clone()))?;
    machine.set_host(Arc::new(Host { job }));
    let status = machine.run();

    let (cwd, env) = machine.top_environment();
    env_bits.cwd = cwd;
    env_bits.env = env;

    status
}

pub fn repl() -> Fallible<()> {
    let mut env = EnvBits {
        cwd: std::env::current_dir()?,
        env: Environment::new(),
        jobs: JobList::default(),
    };

    init_job_control()?;

    let config = Config::builder().history_ignore_space(true).build();

    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(LineEditorHelper {
        completer: FilenameCompleter::new(),
    }));
    rl.load_history("history.txt").ok();

    let mut input = String::new();

    loop {
        let prompt = match input.is_empty() {
            true => "$ ".to_owned(),
            false => "..> ".to_owned(),
        };

        // env.job_list().check_and_print_status();

        // A little bit gross, but the FilenameCompleter implementation
        // uses the process-wide current working dir, so we need to be
        // sure to sync that up with the top level environment in order
        // for tab completion to work as the user expects.
        if std::env::current_dir()?.as_path() != env.cwd {
            std::env::set_current_dir(&env.cwd)?;
        }

        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());

                input.push_str(&line);

                let status = match compile_and_run(&input, &mut env) {
                    Err(e) => {
                        if !is_recoverable_parse_error(&e) {
                            print_error(&e, &input);
                            input.clear();
                        } else {
                            input.push('\n');
                        }
                        continue;
                    }
                    Ok(command) => {
                        input.clear();
                        command
                    }
                };

                put_shell_in_foreground();
            }
            Err(ReadlineError::Interrupted) => {
                input.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                print_error(&err.context("during readline").into(), "");
                break;
            }
        }
    }

    Ok(())
}
