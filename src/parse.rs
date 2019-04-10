//! Shell parser
use failure::{Fail, Fallible};
use shlex::{Aliases, Lexer, Operator, Token, TokenKind, TokenPosition};

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

    pub fn parse(&mut self, aliases: Option<&Aliases>) -> Fallible<Vec<Node>> {
        let mut results = vec![];
        while let Some(cmd) = self.simple_command(aliases)? {
            results.push(Node::SimpleCommand(cmd));
        }
        Ok(results)
    }

    fn simple_command(&mut self, aliases: Option<&Aliases>) -> Fallible<Option<SimpleCommand>> {
        let mut assignments = vec![];
        let mut words = vec![];
        let mut asynchronous = false;

        loop {
            let mut token = self.lexer.next()?;
            eprintln!("token is {:?}", token);
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
                    return Err(ParseErrorKind::UnexpectedToken
                        .context(token.position)
                        .into());
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
pub enum Node {
    SimpleCommand(SimpleCommand),
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

#[cfg(test)]
mod test {
    use super::*;

    fn parse(text: &str, aliases: Option<&Aliases>) -> Fallible<Vec<Node>> {
        let mut parser = Parser::new("test", text.as_bytes());
        parser.parse(aliases)
    }

    #[test]
    fn test_parse() {
        let nodes = parse("ls -l foo", None).unwrap();
        assert_eq!(
            nodes,
            vec![Node::SimpleCommand(SimpleCommand {
                assignments: vec![],
                file_redirects: vec![],
                fd_dups: vec![],
                asynchronous: false,
                words: vec![
                    Token {
                        kind: TokenKind::Word("ls".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                    },
                    Token {
                        kind: TokenKind::Word("-l".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 3
                        },
                    },
                    Token {
                        kind: TokenKind::Word("foo".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 6
                        },
                    }
                ]
            })]
        );
    }

    #[test]
    fn test_parse_two_lines() {
        let nodes = parse("false\ntrue", None).unwrap();
        assert_eq!(
            nodes,
            vec![
                Node::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    file_redirects: vec![],
                    fd_dups: vec![],
                    asynchronous: false,
                    words: vec![Token {
                        kind: TokenKind::Word("false".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                    },]
                }),
                Node::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    file_redirects: vec![],
                    fd_dups: vec![],
                    asynchronous: false,
                    words: vec![Token {
                        kind: TokenKind::Word("true".to_string()),
                        position: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        },
                    },]
                })
            ]
        );
    }

    #[test]
    fn test_parse_with_alias() {
        let mut aliases = Aliases::new();
        aliases.alias("ls", "ls -l");
        let nodes = parse("ls foo", Some(&aliases)).unwrap();
        assert_eq!(
            nodes,
            vec![Node::SimpleCommand(SimpleCommand {
                assignments: vec![],
                file_redirects: vec![],
                fd_dups: vec![],
                asynchronous: false,
                words: vec![
                    Token {
                        kind: TokenKind::Word("ls -l".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                    },
                    Token {
                        kind: TokenKind::Word("foo".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 3
                        },
                    }
                ]
            })]
        );
    }
}
