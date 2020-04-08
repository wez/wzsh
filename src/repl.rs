use crate::errorprint::print_error;
use crate::job::{put_shell_in_foreground, Job, JOB_LIST};
use crate::shellhost::{FunctionRegistry, Host};
use anyhow::{anyhow, Error};
use filenamegen::Glob;
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
fn init_job_control() -> anyhow::Result<()> {
    use anyhow::Context;

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
            return Err(std::io::Error::last_os_error())
                .context("unable to put shell into its own process group");
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
    funcs: Arc<FunctionRegistry>,
}

fn compile_and_run(prog: &str, env_bits: &mut EnvBits) -> anyhow::Result<Status> {
    let job = Job::new_empty(prog.to_owned());
    let mut parser = Parser::new(prog.as_bytes());
    let command = parser.parse()?;
    let mut compiler = Compiler::new();
    compiler.compile_command(&command)?;
    let prog = compiler.finish()?;
    let mut machine = Machine::new(
        &Program::new(prog),
        Some(env_bits.env.clone()),
        &env_bits.cwd,
    )?;
    machine.set_host(Arc::new(Host::with_job_control(job, &env_bits.funcs)));
    let status = machine.run();

    let (cwd, env) = machine.top_environment();
    env_bits.cwd = cwd;
    env_bits.env = env;

    status
}

#[derive(Default)]
struct EditHost {
    history: BasicHistory,
    cwd: PathBuf,
    is_continuation: bool,
}

impl LineEditorHost for EditHost {
    fn render_prompt(&self, _prompt: &str) -> Vec<OutputElement> {
        vec![
            OutputElement::Attribute(AttributeChange::Foreground(AnsiColor::Purple.into())),
            OutputElement::Text(self.cwd.display().to_string()),
            OutputElement::Text("\r\n".into()),
            OutputElement::Attribute(AttributeChange::Foreground(
                ColorAttribute::TrueColorWithPaletteFallback(
                    RgbColor::from_named("skyblue").unwrap(),
                    AnsiColor::Navy.into(),
                ),
            )),
            OutputElement::Text(if self.is_continuation {
                "..> ".to_owned()
            } else {
                "$ ".to_owned()
            }),
        ]
    }

    fn history(&mut self) -> &mut dyn History {
        &mut self.history
    }

    fn complete(&self, line: &str, cursor_position: usize) -> Vec<CompletionCandidate> {
        let mut candidates = vec![];
        if let Some((range, word)) = word_at_cursor(line, cursor_position) {
            if let Ok(glob) = Glob::new(&format!("{}*", word)) {
                for p in glob.walk(&self.cwd) {
                    if let Some(text) = p.to_str() {
                        candidates.push(CompletionCandidate {
                            range: range.clone(),
                            // It is convenient when tabbing to complet dirs
                            // for the directory separator to be included in
                            // the completion text, so we do that here.
                            text: format!("{}{}", text, if p.is_dir() { "/" } else { "" }),
                        });
                    }
                }
            }
        }
        candidates
    }
}

fn word_at_cursor(line: &str, cursor_position: usize) -> Option<(std::ops::Range<usize>, &str)> {
    let char_indices: Vec<(usize, char)> = line.char_indices().collect();
    if char_indices.is_empty() {
        return None;
    }
    let char_position = char_indices
        .iter()
        .position(|(idx, _)| *idx == cursor_position)
        .unwrap_or(char_indices.len());

    // Look back until we find whitespace
    let mut start_position = char_position;
    while start_position > 0
        && start_position <= char_indices.len()
        && !char_indices[start_position - 1].1.is_whitespace()
    {
        start_position -= 1;
    }

    // Look forwards until we find whitespace
    let mut end_position = char_position;
    while end_position < char_indices.len() && !char_indices[end_position].1.is_whitespace() {
        end_position += 1;
    }

    if end_position > start_position {
        let range = char_indices[start_position].0
            ..char_indices
                .get(end_position)
                .map(|c| c.0 + 1)
                .unwrap_or(line.len());
        Some((range.clone(), &line[range]))
    } else {
        None
    }
}

pub fn repl(cwd: PathBuf, env: Environment, funcs: &Arc<FunctionRegistry>) -> anyhow::Result<()> {
    let mut env = EnvBits {
        cwd,
        env,
        funcs: Arc::clone(funcs),
    };

    #[cfg(unix)]
    init_job_control()?;

    let mut editor = line_editor().map_err(Error::msg)?;
    let mut host = EditHost::default();

    let mut input = String::new();

    loop {
        // We handle all the prompt rendering in render_prompt.
        editor.set_prompt("");

        JOB_LIST.check_and_print_status();

        host.cwd = env.cwd.clone();
        host.is_continuation = !input.is_empty();

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
                print_error(&anyhow!("during readline: {}", err), "");
                break;
            }
        }
    }

    Ok(())
}
