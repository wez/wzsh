use failure::{bail, format_err, Error, Fallible};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::process::Command;

use shlex::string::ShellString;
use shlex::{Environment, Expander};
mod parse;
use parse::{Node, Parser};

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

fn main() -> Result<(), Error> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut env = Environment::new();
    let expander = ShellExpander {};
    let aliases = None;

    loop {
        let readline = rl.readline("$ ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());

                let mut parser = Parser::new("stdin", line.as_bytes());
                for node in parser.parse(aliases)? {
                    match node {
                        Node::SimpleCommand(cmd) => {
                            let argv = cmd.expand_argv(&mut env, &expander)?;
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
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
