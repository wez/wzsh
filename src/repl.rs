use crate::errorprint::print_error;
use crate::job::{put_shell_in_foreground, Job, JOB_LIST};
use crate::shellhost::Host;
use failure::{Error, Fail, Fallible};
use shell_compiler::Compiler;
use shell_lexer::{LexError, LexErrorKind};
use shell_parser::{ParseErrorKind, Parser};
use shell_vm::{Environment, Machine, Program, Status};
use std::path::PathBuf;
use std::sync::Arc;
use termwiz::cell::AttributeChange;
use termwiz::color::{AnsiColor, ColorAttribute, RgbColor};
use termwiz::lineedit::*;

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

#[cfg(unix)]
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

struct EnvBits {
    cwd: PathBuf,
    env: Environment,
}

fn compile_and_run(prog: &str, env_bits: &mut EnvBits) -> Fallible<Status> {
    let job = Job::new_empty(prog.to_owned());
    let mut parser = Parser::new(prog.as_bytes());
    let command = parser.parse()?;
    let mut compiler = Compiler::new();
    compiler.compile_command(&command)?;
    let prog = compiler.finish()?;
    let mut machine = Machine::new(&Program::new(prog), Some(env_bits.env.clone()))?;
    machine.set_host(Arc::new(Host::new(job)));
    let status = machine.run();

    let (cwd, env) = machine.top_environment();
    env_bits.cwd = cwd;
    env_bits.env = env;

    status
}

#[derive(Default)]
struct EditHost {
    history: BasicHistory,
}

impl LineEditorHost for EditHost {
    fn render_prompt(&self, prompt: &str) -> Vec<OutputElement> {
        vec![
            OutputElement::Attribute(AttributeChange::Foreground(
                ColorAttribute::TrueColorWithPaletteFallback(
                    RgbColor::from_named("plum").unwrap(),
                    AnsiColor::Navy.into(),
                ),
            )),
            OutputElement::Text(prompt.to_owned()),
        ]
    }

    fn history(&mut self) -> &mut History {
        &mut self.history
    }
}

pub fn repl(cwd: PathBuf, env: Environment) -> Fallible<()> {
    let mut env = EnvBits { cwd, env };

    #[cfg(unix)]
    init_job_control()?;

    let mut editor = line_editor()?;
    let mut host = EditHost::default();

    let mut input = String::new();

    loop {
        let prompt = match input.is_empty() {
            true => "$ ",
            false => "..> ",
        };
        editor.set_prompt(prompt);

        JOB_LIST.check_and_print_status();

        match editor.read_line(&mut host) {
            Ok(Some(line)) => {
                host.history().add(&line);

                input.push_str(&line);

                let _status = match compile_and_run(&input, &mut env) {
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
            Ok(None) => {
                input.clear();
                continue;
            }
            Err(err) => {
                print_error(&err.context("during readline").into(), "");
                break;
            }
        }
    }

    Ok(())
}
