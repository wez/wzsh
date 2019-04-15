use failure::{Error, Fallible};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{Config, Editor, Helper};
use shlex::error::LexError;
use shlex::LexErrorKind;
use std::borrow::Cow;

use crate::errorprint::print_error;
use crate::exitstatus::ExitStatus;
use crate::parse::{ParseErrorKind, Parser};
use crate::{ExecutionEnvironment, ShellExpander};

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

pub fn repl(mut env: ExecutionEnvironment, expander: ShellExpander) -> Fallible<()> {
    let config = Config::builder().history_ignore_space(true).build();

    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(LineEditorHelper {
        completer: FilenameCompleter::new(),
    }));
    rl.load_history("history.txt").ok();

    let mut input = String::new();
    let mut last_status = ExitStatus::new_ok();

    loop {
        let prompt = match (input.is_empty(), last_status) {
            (true, st) if !st.is_ok() => format!("[{}] $ ", st.code()),
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
                    Ok(status) => status.wait()?,
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
