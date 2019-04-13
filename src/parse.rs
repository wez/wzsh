//! Shell parser
use failure::{bail, Fail, Fallible};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, Lexer, Operator, Token, TokenKind};

#[derive(Debug, Clone, Copy, Fail)]
pub enum ParseErrorKind {
    #[fail(display = "Unexpected token")]
    UnexpectedToken,
}

pub struct Parser<R: std::io::Read> {
    lexer: Lexer<R>,
}

impl<R: std::io::Read> Parser<R> {
    pub fn new(source: &str, stream: R) -> Self {
        let lexer = Lexer::new(source, stream);
        Self { lexer }
    }

    pub fn parse(&mut self, aliases: Option<&Aliases>) -> Fallible<CompoundList> {
        let mut commands = vec![];
        while let Some(cmd) = self.simple_command(aliases)? {
            commands.push(Command::SimpleCommand(cmd));
        }
        Ok(CompoundList { commands })
    }

    fn simple_command(&mut self, aliases: Option<&Aliases>) -> Fallible<Option<SimpleCommand>> {
        let mut assignments = vec![];
        let mut words = vec![];
        let mut asynchronous = false;

        loop {
            let mut token = self.lexer.next()?;
            match token.kind {
                TokenKind::Eof => break,
                TokenKind::Operator(Operator::Ampersand) => {
                    asynchronous = true;
                    break;
                }
                TokenKind::Operator(Operator::Semicolon) | TokenKind::NewLine => {
                    break;
                }
                TokenKind::Word(_) => {
                    if words.is_empty() && token.kind.parse_assignment_word().is_some() {
                        assignments.push(token);
                    } else if words.is_empty() {
                        // Command word
                        token.apply_command_word_rules(aliases);
                        words.push(token);
                    } else {
                        words.push(token);
                    }
                }

                _ => {
                    return Err(ParseErrorKind::UnexpectedToken.context(token.start).into());
                }
            }
        }

        if assignments.is_empty() && words.is_empty() {
            return Ok(None);
        }

        Ok(Some(SimpleCommand {
            assignments,
            file_redirects: vec![],
            fd_dups: vec![],
            words,
            asynchronous,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    SimpleCommand(SimpleCommand),
    CompoundCommand(CompoundCommand, Option<RedirectList>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RedirectList {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundCommand {
    BraceGroup(CompoundList),
    Subshell(CompoundList),
    ForEach(ForEach),
    If(If),
    UntilLoop(UntilLoop),
    WhileLoop(WhileLoop),
    // TODO: Case
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipeline {
    /// true if the pipeline starts with a bang
    inverted: bool,
    commands: Vec<Command>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundList {
    commands: Vec<Command>,
}

impl IntoIterator for CompoundList {
    type Item = Command;
    type IntoIter = ::std::vec::IntoIter<Command>;
    fn into_iter(self) -> Self::IntoIter {
        self.commands.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    condition: CompoundList,
    true_part: CompoundList,
    false_part: Option<CompoundList>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UntilLoop {
    body: CompoundList,
    condition: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
    condition: CompoundList,
    body: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForEach {
    wordlist: Vec<Token>,
    body: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileRedirection {
    pub fd_number: usize,
    pub file_name: Token,
    /// `<` or `<>`
    pub input: bool,
    /// `>` or `<>`
    pub output: bool,
    /// `>|`
    pub clobber: bool,
    /// `>>`
    pub append: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FdDuplication {
    /// Dup `src_fd_number` ...
    pub src_fd_number: usize,
    /// ... into `dest_fd_number` for the child
    pub dest_fd_number: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleCommand {
    /// Any assignment words to override the environment
    assignments: Vec<Token>,
    file_redirects: Vec<FileRedirection>,
    fd_dups: Vec<FdDuplication>,
    /// The words that will be expanded to form the argv
    words: Vec<Token>,
    /// true if `&` was used as the separator between
    /// commands in the containing list
    asynchronous: bool,
}

impl SimpleCommand {
    pub fn expand_argv(
        &self,
        env: &mut Environment,
        expander: &Expander,
    ) -> Fallible<Vec<ShellString>> {
        // FIXME: scoped assignments need to return a new env
        let mut argv = vec![];
        for word in &self.words {
            match word.kind {
                TokenKind::Word(ref s) | TokenKind::Name(ref s) => {
                    let mut fields = expander.expand_word(&s.as_str().into(), env)?;
                    argv.append(&mut fields);
                }
                _ => bail!("unhandled token kind {:?}", word),
            }
        }
        Ok(argv)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use shlex::TokenPosition;

    fn parse(text: &str, aliases: Option<&Aliases>) -> Fallible<CompoundList> {
        let mut parser = Parser::new("test", text.as_bytes());
        parser.parse(aliases)
    }

    #[test]
    fn test_parse() {
        let nodes = parse("ls -l foo", None).unwrap();
        assert_eq!(
            nodes,
            CompoundList {
                commands: vec![Command::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    file_redirects: vec![],
                    fd_dups: vec![],
                    asynchronous: false,
                    words: vec![
                        Token {
                            kind: TokenKind::Word("ls".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 0
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 1
                            },
                        },
                        Token {
                            kind: TokenKind::Word("-l".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 3
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 4
                            },
                        },
                        Token {
                            kind: TokenKind::Word("foo".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 6
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 8
                            },
                        }
                    ]
                })]
            }
        );
    }

    #[test]
    fn test_parse_two_lines() {
        let nodes = parse("false\ntrue", None).unwrap();
        assert_eq!(
            nodes,
            CompoundList {
                commands: vec![
                    Command::SimpleCommand(SimpleCommand {
                        assignments: vec![],
                        file_redirects: vec![],
                        fd_dups: vec![],
                        asynchronous: false,
                        words: vec![Token {
                            kind: TokenKind::Word("false".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 0
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 4
                            },
                        },]
                    }),
                    Command::SimpleCommand(SimpleCommand {
                        assignments: vec![],
                        file_redirects: vec![],
                        fd_dups: vec![],
                        asynchronous: false,
                        words: vec![Token {
                            kind: TokenKind::Word("true".to_string()),
                            start: TokenPosition {
                                line_number: 1,
                                col_number: 0
                            },
                            end: TokenPosition {
                                line_number: 1,
                                col_number: 3
                            },
                        },]
                    })
                ]
            }
        );
    }

    #[test]
    fn test_parse_with_alias() {
        let mut aliases = Aliases::new();
        aliases.alias("ls", "ls -l");
        let nodes = parse("ls foo", Some(&aliases)).unwrap();
        assert_eq!(
            nodes,
            CompoundList {
                commands: vec![Command::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    file_redirects: vec![],
                    fd_dups: vec![],
                    asynchronous: false,
                    words: vec![
                        Token {
                            kind: TokenKind::Word("ls -l".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 0
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 1
                            },
                        },
                        Token {
                            kind: TokenKind::Word("foo".to_string()),
                            start: TokenPosition {
                                line_number: 0,
                                col_number: 3
                            },
                            end: TokenPosition {
                                line_number: 0,
                                col_number: 5
                            },
                        }
                    ]
                })]
            }
        );
    }
}
