use crate::errors::LexErrorKind;
use crate::position::{Pos, Span};
use crate::reader::{CharReader, Next, PositionedChar};
use crate::tokenenum::MatchResult;
use crate::{Operator, OPERATORS};
use failure::{bail, Fallible};
use lazy_static::lazy_static;
use regex::Regex;
use std::io::Read;

lazy_static! {
    static ref TILE_EXPAND_RE: Regex =
        Regex::new(r"^~([a-zA-Z_][a-zA-Z0-9_]+)?(/|$)").expect("failed to compile TILE_EXPAND_RE");
}

#[derive(Debug, PartialEq, Eq)]
pub enum WordComponentKind {
    Literal(String),
    TildeExpand(Option<String>),
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

#[derive(Debug, PartialEq, Eq)]
enum State {
    Top,
    AssignmentWord,
}

#[derive(Debug)]
struct LexState {
    state: State,
    current_word: Option<Vec<WordComponent>>,
}

pub struct Lexer<R: Read> {
    reader: CharReader<R>,
    stack: Vec<LexState>,
}

impl<R: Read> Lexer<R> {
    pub fn new(stream: R) -> Self {
        Self {
            reader: CharReader::new(stream),
            stack: vec![LexState {
                state: State::Top,
                current_word: None,
            }],
        }
    }

    pub fn next_token(&mut self) -> Fallible<Token> {
        eprintln!("calling next_token");
        match self.state().state {
            State::Top => self.top(),
            State::AssignmentWord => self.assignment_word(),
        }
    }

    fn push_state(&mut self, state: State) {
        self.stack.push(LexState {
            state,
            current_word: None,
        });
    }

    fn pop_state(&mut self) {
        self.stack.pop();
    }

    fn word_common(&mut self) -> Fallible<Option<Token>> {
        match self.reader.next_char() {
            Next::Eof(pos) => {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(Some(token));
                }
                return Ok(Some(Token::Eof(pos)));
            }
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            Next::Char(c) => {
                if c.c == ' ' || c.c == '\t' || c.c == '\r' {
                    if let Some(token) = self.delimit_current_word() {
                        return Ok(Some(token));
                    }
                } else if c.c == '\n' {
                    if let Some(token) = self.delimit_current_word() {
                        self.reader.unget(c);
                        return Ok(Some(token));
                    }
                    return Ok(Some(Token::Newline(c.pos)));
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
        };
        Ok(None)
    }

    fn assignment_word(&mut self) -> Fallible<Token> {
        loop {
            if let Some(token) = self.word_common()? {
                self.pop_state();
                return Ok(token);
            }
        }
    }

    fn top(&mut self) -> Fallible<Token> {
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
                self.push_state(State::AssignmentWord);
                let value_token = self.assignment_word()?;
                match value_token {
                    Token::Word(value) => {
                        return Ok(Token::Assignment { name, span, value });
                    }
                    _ => {
                        bail!("unexpected value token {:?}", value_token);
                    }
                }
            }

            if let Some(token) = self.word_common()? {
                return Ok(token);
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

    fn state(&mut self) -> &mut LexState {
        self.stack.last_mut().unwrap()
    }

    fn delimit_current_word(&mut self) -> Option<Token> {
        let state = self.state();
        if let Some(mut word) = state.current_word.take() {
            // Check to see if we can apply tilde expansion before we return
            let last = if state.state == State::AssignmentWord {
                split_first_word_by_unquoted_colons(&mut word)
            } else {
                0
            };
            eprintln!(
                "delimit: state={:?}, last={} word={:?}",
                state.state, last, word
            );
            for i in (0..=last).rev() {
                apply_single_tilde_expansion(i, &mut word);
            }

            Some(Token::Word(word))
        } else {
            None
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
        let state = self.state();
        if state.current_word.is_none() {
            state.current_word = Some(vec![]);
        }
        state.current_word.as_mut().unwrap().push(word);
    }

    fn add_char_to_word(&mut self, c: PositionedChar) {
        let state = self.state();
        if state.current_word.is_none() {
            state.current_word = Some(vec![]);
        }
        let current = state.current_word.as_mut().unwrap();
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

fn apply_single_tilde_expansion(word_idx: usize, words: &mut Vec<WordComponent>) {
    eprintln!("apply_single_tilde_expansion word_idx={}", word_idx);
    if let WordComponent {
        kind: WordComponentKind::Literal(first_word),
        splittable: true,
        remove_backslash: true,
        span,
    } = &words[word_idx]
    {
        if let Some(caps) = TILE_EXPAND_RE.captures(first_word) {
            let all = caps.get(0).unwrap();
            let trailer = caps.get(2).unwrap();
            let matched_len = if trailer.as_str().len() == 1 {
                // We want to include the `/` in the remainder
                all.end() - 1
            } else {
                all.end()
            };
            let remainder = first_word[matched_len..].to_string();
            let name = caps.get(1).map(|cap| cap.as_str().to_owned());
            let span = span.clone();

            words.remove(word_idx);

            let start = span.start;
            let mut end = start;
            end.col = start.col + name.as_ref().map(|n| n.len()).unwrap_or(0);

            words.insert(
                word_idx,
                WordComponent {
                    kind: WordComponentKind::TildeExpand(name),
                    splittable: false,
                    remove_backslash: false,
                    span: Span::new(start, end),
                },
            );

            if !remainder.is_empty() {
                let mut rem_start = end;
                rem_start.col += 1;
                let mut rem_end = rem_start;
                rem_end.col += remainder.len() - 1;

                words.insert(
                    word_idx + 1,
                    WordComponent {
                        kind: WordComponentKind::Literal(remainder),
                        splittable: true,
                        remove_backslash: true,
                        span: Span::new(rem_start, rem_end),
                    },
                );
            }
        }
    }
}

fn split_first_word_by_unquoted_colons(words: &mut Vec<WordComponent>) -> usize {
    if let WordComponent {
        kind: WordComponentKind::Literal(first_word),
        splittable: true,
        remove_backslash: true,
        span,
    } = &words[0]
    {
        let first_word = first_word.to_string();
        let mut start = span.start;
        words.remove(0);
        let mut prev = None;
        let mut result = 0;
        for (idx, element) in first_word
            .split(|c| match prev.replace(c) {
                Some('\\') => false,
                _ => c == ':',
            })
            .enumerate()
        {
            if idx > 0 {
                words.insert(
                    result,
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        splittable: false,
                        remove_backslash: false,
                        span: Span::new(start, start),
                    },
                );
                result += 1;
                start.col += 1;
            }
            let mut end = start;
            end.col += element.len() - 1;
            words.insert(
                result,
                WordComponent {
                    kind: WordComponentKind::literal(element),
                    splittable: true,
                    remove_backslash: true,
                    span: Span::new(start, end),
                },
            );
            result += 1;
            start.col = end.col + 1;
        }

        result - 1
    } else {
        0
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
            tokens("FOO=~bar"),
            vec![Token::Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::TildeExpand(Some("bar".to_owned())),
                    span: Span::new_to(0, 4, 7),
                    splittable: false,
                    remove_backslash: false
                }]
            }]
        );

        assert_eq!(
            tokens("FOO=~bar:/somewhere:~foo/baz"),
            vec![Token::Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![
                    WordComponent {
                        kind: WordComponentKind::TildeExpand(Some("bar".to_owned())),
                        span: Span::new_to(0, 4, 7),
                        splittable: false,
                        remove_backslash: false
                    },
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        span: Span::new_to(0, 8, 8),
                        splittable: false,
                        remove_backslash: false,
                    },
                    WordComponent {
                        kind: WordComponentKind::literal("/somewhere"),
                        span: Span::new_to(0, 9, 18),
                        splittable: true,
                        remove_backslash: true,
                    },
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        span: Span::new_to(0, 19, 19),
                        splittable: false,
                        remove_backslash: false,
                    },
                    WordComponent {
                        kind: WordComponentKind::TildeExpand(Some("foo".to_owned())),
                        span: Span::new_to(0, 20, 23),
                        splittable: false,
                        remove_backslash: false
                    },
                    WordComponent {
                        kind: WordComponentKind::literal("/baz"),
                        span: Span::new_to(0, 24, 27),
                        splittable: true,
                        remove_backslash: true,
                    },
                ]
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

    #[test]
    fn tilde() {
        assert_eq!(
            tokens("~"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::TildeExpand(None),
                span: Span::new_to(0, 0, 0),
                splittable: false,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("~wez"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::TildeExpand(Some("wez".to_owned())),
                span: Span::new_to(0, 0, 3),
                splittable: false,
                remove_backslash: false,
            }])]
        );

        assert_eq!(
            tokens("~/"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::TildeExpand(None),
                    span: Span::new_to(0, 0, 0),
                    splittable: false,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("/"),
                    span: Span::new_to(0, 1, 1),
                    splittable: true,
                    remove_backslash: true
                },
            ])]
        );
        assert_eq!(
            tokens("~wez/"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::TildeExpand(Some("wez".to_owned())),
                    span: Span::new_to(0, 0, 3),
                    splittable: false,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("/"),
                    span: Span::new_to(0, 4, 4),
                    splittable: true,
                    remove_backslash: true
                },
            ])]
        );

        assert_eq!(
            tokens("~:"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("~:"),
                span: Span::new_to(0, 0, 1),
                splittable: true,
                remove_backslash: true
            },])]
        );
        assert_eq!(
            tokens("\\~"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("\\~"),
                span: Span::new_to(0, 0, 1),
                splittable: true,
                remove_backslash: true
            },])]
        );
    }
}
