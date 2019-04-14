use failure::{bail, ensure, format_err, Error, Fail, Fallible};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{Config, Editor, Helper};
use std::borrow::Cow;
use std::process::Command;

use shlex::error::{LexError, TokenPosition};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, LexErrorKind, TokenKind};
mod parse;
use parse::{CommandType, CompoundList, FileRedirection, ParseErrorKind, Parser, Redirection};

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

fn eval(
    env: &mut Environment,
    expander: &ShellExpander,
    list: CompoundList,
    aliases: &Aliases,
) -> Fallible<()> {
    for command in list {
        match command.command {
            CommandType::SimpleCommand(cmd) => {
                let argv = cmd.expand_argv(env, expander, aliases)?;
                if !argv.is_empty() {
                    let mut child_cmd = Command::new(&argv[0]);
                    for arg in argv.iter().skip(1) {
                        child_cmd.arg(arg);
                    }
                    child_cmd.env_clear();
                    child_cmd.envs(env.iter());

                    for redir in &cmd.redirections {
                        match redir {
                            Redirection::File(FileRedirection {
                                fd_number,
                                file_name,
                                input,
                                output,
                                clobber,
                                append,
                            }) => {
                                ensure!(
                                    *fd_number < 3,
                                    "only stdin, stdout, stderr currently support for redirection"
                                );
                                let file_name = match &file_name.kind {
                                    TokenKind::Word(word) => {
                                        let word = ShellString::from(word.as_str());
                                        let file = expander.expand_word(&word, env)?;
                                        ensure!(
                                            file.len() == 1,
                                            "{:?} expanded to {:?}, expected just a single item",
                                            file_name,
                                            file
                                        );
                                        file.into_iter().next().unwrap()
                                    }
                                    _ => bail!("file_name is not a word token"),
                                };
                                let mut options = std::fs::OpenOptions::new();
                                options
                                    .read(*input)
                                    .write(*output)
                                    .append(*append)
                                    .truncate(*output && !*append)
                                    // TODO: if a noclobber option is set, and !*clobber,
                                    // then we should look at .create_new() instead
                                    .create(*output || *clobber);
                                let file = match options.open(file_name.as_ref()) {
                                    Ok(file) => file,
                                    Err(e) => {
                                        return Err(e
                                            .context(format!(
                                                "opening '{}' using {:#?}",
                                                file_name, options
                                            ))
                                            .into());
                                    }
                                };

                                match fd_number {
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

                    let mut child = child_cmd.spawn()?;
                    child.wait()?;
                }
            }
            _ => bail!("eval doesn't know about {:#?}", command),
        }
    }
    Ok(())
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

    let mut env = Environment::new();
    let expander = ShellExpander {};
    let aliases = Aliases::new();

    let mut input = String::new();

    loop {
        let readline = rl.readline(if input.is_empty() { "$ " } else { "..> " });
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
                if let Err(e) = eval(&mut env, &expander, list, &aliases) {
                    print_error(&e, &input);
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
