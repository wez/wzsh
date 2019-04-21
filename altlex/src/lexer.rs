use crate::errors::LexErrorKind;
use crate::position::{Pos, Span};
use crate::reader::{CharReader, Next, PositionedChar};
use crate::tokenenum::MatchResult;
use crate::{Operator, OPERATORS};
use failure::Fallible;
use std::io::Read;

#[derive(Debug, PartialEq, Eq)]
pub enum WordComponentKind {
    Literal(String),
}

impl WordComponentKind {
    pub fn literal(s: &str) -> WordComponentKind {
        WordComponentKind::Literal(s.to_owned())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WordComponent {
    pub kind: WordComponentKind,
    pub span: Span,
    pub splittable: bool,
    pub remove_backslash: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(Vec<WordComponent>),
    Operator(Operator, Span),
    Eof(Pos),
    Newline(Pos),
    IoNumber(usize, Span),
    Assignment {
        name: String,
        span: Span,
        value: Vec<WordComponent>,
    },
}

pub struct Lexer<R: Read> {
    reader: CharReader<R>,
    current_word: Option<Vec<WordComponent>>,
}

impl<R: Read> Lexer<R> {
    pub fn new(stream: R) -> Self {
        Self {
            reader: CharReader::new(stream),
            current_word: None,
        }
    }

    fn delimit_current_word(&mut self) -> Option<Token> {
        if let Some(word) = self.current_word.take() {
            Some(Token::Word(word))
        } else {
            None
        }
    }

    /// Read tokens until we delimit the next word; return
    /// that word.
    fn next_word(&mut self, kind: LexErrorKind) -> Fallible<Token> {
        assert!(self.current_word.is_none());

        loop {
            match self.reader.next_char() {
                Next::Eof(pos) => {
                    if let Some(token) = self.delimit_current_word() {
                        return Ok(token);
                    }
                    return Err(kind.at(pos.into()).into());
                }
                Next::Error(err, pos) => return Err(err.context(pos).into()),
                Next::Char(c) => {
                    if c.c == ' ' || c.c == '\t' || c.c == '\r' || c.c == '\n' {
                        if let Some(token) = self.delimit_current_word() {
                            return Ok(token);
                        }
                        return Err(kind.at(c.pos.into()).into());
                    } else if c.c == '\'' {
                        self.single_quotes(c.pos)?;
                    } else if c.c == '"' {
                        self.double_quotes(c.pos)?;
                    } else if c.c == '\\' {
                        self.backslash(c)?;
                    } else {
                        self.add_char_to_word(c);
                    }
                }
            }
        }
    }

    pub fn next_token(&mut self) -> Fallible<Token> {
        eprintln!("calling next_token");
        loop {
            if let MatchResult::Match(..) = self.reader.matches_literal(&OPERATORS)? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }

                let (op, span) = self.reader.next_literal(&OPERATORS)?.unwrap();
                return Ok(Token::Operator(op, span));
            }

            if self.reader.matches_io_number()? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }
                let (num, span) = self.reader.next_io_number()?.unwrap();
                return Ok(Token::IoNumber(num, span));
            }

            if self.reader.matches_assignment_word()? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }
                let (name, span) = self.reader.next_assignment_word()?.unwrap();
                if let Token::Word(value) = self.next_word(LexErrorKind::EofDuringAssignmentWord)? {
                    return Ok(Token::Assignment { name, span, value });
                }
            }

            match self.reader.next_char() {
                Next::Eof(pos) => {
                    if let Some(token) = self.delimit_current_word() {
                        return Ok(token);
                    }
                    return Ok(Token::Eof(pos));
                }
                Next::Error(err, pos) => return Err(err.context(pos).into()),
                Next::Char(c) => {
                    if c.c == ' ' || c.c == '\t' || c.c == '\r' {
                        if let Some(token) = self.delimit_current_word() {
                            return Ok(token);
                        }
                    } else if c.c == '\n' {
                        if let Some(token) = self.delimit_current_word() {
                            self.reader.unget(c);
                            return Ok(token);
                        }
                        return Ok(Token::Newline(c.pos));
                    } else if c.c == '\'' {
                        self.single_quotes(c.pos)?;
                    } else if c.c == '"' {
                        self.double_quotes(c.pos)?;
                    } else if c.c == '\\' {
                        self.backslash(c)?;
                    } else {
                        self.add_char_to_word(c);
                    }
                }
            }
        }
    }

    fn next_char_or_err(&mut self, err: LexErrorKind) -> Fallible<PositionedChar> {
        match self.reader.next_char() {
            Next::Char(b) => Ok(b),
            Next::Eof(pos) => Err(err.at(pos.into()).into()),
            Next::Error(e, pos) => Err(e.context(pos).into()),
        }
    }

    fn backslash(&mut self, backslash: PositionedChar) -> Fallible<()> {
        let quoted = self.next_char_or_err(LexErrorKind::EofDuringBackslash)?;
        self.add_char_to_word(backslash);
        self.add_char_to_word(quoted);
        Ok(())
    }

    fn single_quotes(&mut self, start: Pos) -> Fallible<()> {
        let mut accumulator = String::new();
        let mut end;
        loop {
            let c = self.next_char_or_err(LexErrorKind::EofDuringSingleQuotedString)?;
            end = c.pos;
            if c.c == '\'' {
                break;
            }
            accumulator.push(c.c);
        }

        let word = WordComponent {
            kind: WordComponentKind::Literal(accumulator),
            span: Span::new(start, end),
            splittable: false,
            remove_backslash: false,
        };

        self.add_to_word(word);
        Ok(())
    }

    fn double_quotes(&mut self, start: Pos) -> Fallible<()> {
        let mut accumulator = String::new();
        let mut backslash = false;
        let mut end;
        loop {
            let c = self.next_char_or_err(LexErrorKind::EofDuringDoubleQuotedString)?;
            end = c.pos;
            if c.c == '"' && !backslash {
                break;
            }
            backslash = false;
            if c.c == '\\' {
                backslash = true;
            }

            // TODO: check for parameter and command expansion

            accumulator.push(c.c);
        }

        let word = WordComponent {
            kind: WordComponentKind::Literal(accumulator),
            span: Span::new(start, end),
            splittable: false,
            remove_backslash: true,
        };

        self.add_to_word(word);
        Ok(())
    }

    fn add_to_word(&mut self, word: WordComponent) {
        if self.current_word.is_none() {
            self.current_word = Some(vec![]);
        }
        self.current_word.as_mut().unwrap().push(word);
    }

    fn add_char_to_word(&mut self, c: PositionedChar) {
        if self.current_word.is_none() {
            self.current_word = Some(vec![]);
        }
        let current = self.current_word.as_mut().unwrap();
        match current.last_mut() {
            Some(WordComponent {
                kind: WordComponentKind::Literal(s),
                span,
                splittable: true,
                remove_backslash: true,
            }) => {
                s.push(c.c);
                span.end = c.pos;
            }
            _ => {
                current.push(WordComponent {
                    kind: WordComponentKind::Literal(c.c.to_string()),
                    span: Span::new(c.pos, c.pos),
                    splittable: true,
                    remove_backslash: true,
                });
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn tokens(s: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(s.as_bytes());
        let mut tokens = vec![];
        loop {
            match lexer.next_token().unwrap() {
                Token::Eof(_) => break,
                token @ _ => tokens.push(token),
            }
        }
        eprintln!("tokens: {:#?}", tokens);
        tokens
    }

    #[test]
    fn operator() {
        assert_eq!(
            tokens("||"),
            vec![Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1))]
        );
        assert_eq!(
            tokens("||&&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::AndIf, Span::new_to(0, 2, 3))
            ]
        );
        assert_eq!(
            tokens("|| &&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::AndIf, Span::new_to(0, 3, 4))
            ]
        );
        assert_eq!(
            tokens("||\n&&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Newline(Pos::new(0, 2)),
                Token::Operator(Operator::AndIf, Span::new_to(1, 0, 1))
            ]
        );
    }

    #[test]
    fn single_quotes() {
        assert_eq!(
            tokens("'foo'\n&&"),
            vec![
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 0, 4),
                    splittable: false,
                    remove_backslash: false
                },]),
                Token::Newline(Pos::new(0, 5)),
                Token::Operator(Operator::AndIf, Span::new_to(1, 0, 1))
            ]
        );
    }

    #[test]
    fn double_quotes() {
        assert_eq!(
            tokens("\"fo\\o\"bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::literal("fo\\o"),
                    span: Span::new_to(0, 0, 5),
                    splittable: false,
                    remove_backslash: true
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 6, 8),
                    splittable: true,
                    remove_backslash: true
                },
            ]),]
        );
    }

    #[test]
    fn backslash() {
        assert_eq!(
            tokens("fo\\o"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("fo\\o"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            },]),]
        );
    }

    #[test]
    fn io_number() {
        assert_eq!(
            tokens("1<foo"),
            vec![
                Token::IoNumber(1, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::Less, Span::new_to(0, 1, 1)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 2, 4),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
        assert_eq!(
            tokens("1<<foo"),
            vec![
                Token::IoNumber(1, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::DoubleLess, Span::new_to(0, 1, 2)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 3, 5),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
        assert_eq!(
            tokens("0>foo"),
            vec![
                Token::IoNumber(0, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::Great, Span::new_to(0, 1, 1)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 2, 4),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
    }

    #[test]
    fn assignment_word() {
        assert_eq!(
            tokens("FOO=bar"),
            vec![Token::Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 4, 6),
                    splittable: true,
                    remove_backslash: true
                }]
            }]
        );

        assert_eq!(
            tokens("FOO=bar baz"),
            vec![
                Token::Assignment {
                    name: "FOO".to_owned(),
                    span: Span::new_to(0, 0, 4),
                    value: vec![WordComponent {
                        kind: WordComponentKind::literal("bar"),
                        span: Span::new_to(0, 4, 6),
                        splittable: true,
                        remove_backslash: true
                    }]
                },
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("baz"),
                    span: Span::new_to(0, 8, 10),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );

        assert_eq!(
            tokens("FOO=\"bar baz\""),
            vec![Token::Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::literal("bar baz"),
                    span: Span::new_to(0, 4, 12),
                    splittable: false,
                    remove_backslash: true
                }]
            },]
        );
    }
}
