use failure::{bail, format_err, Error, Fallible};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{Config, Editor, Helper};
use std::borrow::Cow;
use std::process::Command;

use shlex::string::ShellString;
use shlex::{Environment, Expander};
mod parse;
use parse::{Command as ShellCommand, CompoundList, Parser};

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

fn eval(env: &mut Environment, expander: &ShellExpander, list: CompoundList) -> Fallible<()> {
    for command in list {
        match command {
            ShellCommand::SimpleCommand(cmd) => {
                let argv = cmd.expand_argv(env, expander)?;
                if !argv.is_empty() {
                    let mut cmd = Command::new(&argv[0]);
                    for arg in argv.iter().skip(1) {
                        cmd.arg(arg);
                    }
                    cmd.env_clear();
                    cmd.envs(env.iter());
                    let mut child = cmd.spawn()?;
                    child.wait()?;
                }
            }
            _ => bail!("eval doesn't know about {:?}", command),
        }
    }
    Ok(())
}

fn main() -> Result<(), Error> {
    let config = Config::builder().history_ignore_space(true).build();

    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(LineEditorHelper {
        completer: FilenameCompleter::new(),
    }));
    rl.load_history("history.txt").ok();

    let mut env = Environment::new();
    let expander = ShellExpander {};
    let aliases = None;

    let mut input = String::new();

    loop {
        let readline = rl.readline(if input.is_empty() { "$ " } else { "..> " });
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());

                input.push_str(&line);

                let mut parser = Parser::new("stdin", input.as_bytes());
                let nodes = match parser.parse(aliases) {
                    Err(_) => {
                        // If we get a parse error, it is probably because
                        // something is incomplete.  Let's keep it buffered
                        // up in the input vector and allow the user to
                        // complete it on the next line, or press ctrl-c
                        // to cancel and clear it.
                        // eprintln!("{}", e);
                        continue;
                    }
                    Ok(nodes) => {
                        input.clear();
                        nodes
                    }
                };
                if let Err(e) = eval(&mut env, &expander, nodes) {
                    eprintln!("{}", e);
                }
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
