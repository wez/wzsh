//! Shell parser
use shlex::{Error, Lexer, Operator, Token, TokenKind, TokenPosition};

pub struct Parser<R: std::io::Read> {
    lexer: Lexer<R>,
}

impl<R: std::io::Read> Parser<R> {
    pub fn new(source: &str, stream: R) -> Self {
        let lexer = Lexer::new(source, stream);
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Vec<Node>, Error> {
        let mut results = vec![];
        while let Some(cmd) = self.simple_command()? {
            results.push(Node::SimpleCommand(cmd));
        }
        Ok(results)
    }

    fn simple_command(&mut self) -> Result<Option<SimpleCommand>, Error> {
        let mut assignments_done = false;
        let mut assignments = vec![];
        let mut words = vec![];
        let mut asynchronous = false;

        loop {
            let token = self.lexer.next()?;
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
                TokenKind::Word(ref word) => {
                    if !assignments_done && word.contains(&b'=') {
                        assignments.push(token);
                    } else {
                        assignments_done = false;
                        // FIXME: if words.is_empty(), rules 7a, 7b
                        words.push(token);
                    }
                }

                _ => {
                    return Err(Error::with_message(
                        &format!("Unexpected token {:?}", token),
                        token.position,
                    ));
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

    fn parse(text: &str) -> Result<Vec<Node>, Error> {
        let mut parser = Parser::new("test", text.as_bytes());
        parser.parse()
    }

    #[test]
    fn test_parse() {
        let nodes = parse("ls -l foo").unwrap();
        assert_eq!(
            nodes,
            vec![Node::SimpleCommand(SimpleCommand {
                assignments: vec![],
                file_redirects: vec![],
                fd_dups: vec![],
                asynchronous: false,
                words: vec![
                    Token {
                        kind: TokenKind::Word(b"ls".to_vec()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                    },
                    Token {
                        kind: TokenKind::Word(b"-l".to_vec()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 3
                        },
                    },
                    Token {
                        kind: TokenKind::Word(b"foo".to_vec()),
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
        let nodes = parse("false\ntrue").unwrap();
        assert_eq!(
            nodes,
            vec![
                Node::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    file_redirects: vec![],
                    fd_dups: vec![],
                    asynchronous: false,
                    words: vec![Token {
                        kind: TokenKind::Word(b"false".to_vec()),
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
                        kind: TokenKind::Word(b"true".to_vec()),
                        position: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        },
                    },]
                })
            ]
        );
    }
}
