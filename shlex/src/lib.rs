//! Shell lexer
//! See https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html

use failure::{Error, Fallible};
use lazy_static::lazy_static;
use regex::{self, Regex};
use std::collections::HashMap;
use std::io::{BufRead, BufReader};

pub mod alias;
pub mod environment;
pub mod error;
pub mod expander;
pub mod paramexp;
pub mod string;
pub use alias::Aliases;
pub use environment::Environment;
pub use error::{LexErrorKind, TokenPosition};
pub use expander::Expander;

pub struct Lexer<R: std::io::Read> {
    source: String,
    stream: BufReader<R>,
    stream_buffer: String,
    stream_buffer_pos: usize,
    stream_position: TokenPosition,
    token_text: String,
    tokens: Vec<PositionedChar>,
}

macro_rules! TokenEnum {
    ($Enum:ident, $Matcher:ident, $(
            $text:literal : $variant:ident
        ),+) => {

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum $Enum {
    $(
        $variant
    ),+
}
lazy_static! {
    static ref $Matcher: LiteralMatcher<$Enum> = {
        LiteralMatcher::new(&[
            $(
                ($text, $Enum::$variant)
            ),+
        ])
    };
}
    }
}

TokenEnum!(
    Operator,
    OPERATORS,
    "<<-": DoubleLessDash,
    "<<": DoubleLess,
    "<&": LessAnd,
    "<>": LessGreat,
    ">>": DoubleGreat,
    ">|": Clobber,
    ">&": GreatAnd,
    "&&": AndIf,
    "||": OrIf,
    ";;": DoubleSemicolon,
    "<": Less,
    "&": Ampersand,
    "|": Pipe,
    ";": Semicolon,
    ">": Great,
    "(": LeftParen,
    ")": RightParen
);

TokenEnum!(
    ReservedWord,
    RESERVED_WORDS,
    "if": If,
    "then": Then,
    "else": Else,
    "elif": Elif,
    "fi": Fi,
    "do": Do,
    "done": Done,
    "case": Case,
    "esac": Esac,
    "while": While,
    "until": Until,
    "for": For,
    "{": LeftBrace,
    "}": RightBrace,
    "!": Bang,
    "in": In
);

#[derive(Debug)]
struct LiteralMatcher<T: Copy> {
    re: Regex,
    map: HashMap<&'static str, T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MatchResult<T: Copy> {
    Match(T, usize),
    No,
}

impl<T: Copy> LiteralMatcher<T> {
    fn new(literals: &[(&'static str, T)]) -> Self {
        let mut pattern = String::new();
        let mut map = HashMap::new();
        pattern.push_str("^(");
        for (idx, lit) in literals.iter().enumerate() {
            if idx > 0 {
                pattern.push('|');
            }
            pattern.push_str(&regex::escape(lit.0));
            map.insert(lit.0, lit.1);
        }
        pattern.push_str(")$");

        Self {
            re: Regex::new(&pattern).unwrap(),
            map,
        }
    }

    fn matches(&self, text: &str) -> MatchResult<T> {
        if let Some(m) = self.re.find(text) {
            MatchResult::Match(*self.map.get(m.as_str()).unwrap(), m.as_str().len())
        } else {
            MatchResult::No
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IoNumber {
    fd_num: usize,
    matched_len: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Eof,
    Operator(Operator),
    ReservedWord(ReservedWord),
    Name(String),
    Word(String),
    IoNumber(usize),
    NewLine,
}

pub fn is_name_char(c: char) -> bool {
    if c >= '0' && c <= '9' {
        true
    } else if c >= 'a' && c <= 'z' {
        true
    } else if c >= 'A' && c <= 'Z' {
        true
    } else if c == '_' {
        true
    } else {
        false
    }
}

/// In the shell command language, a word consisting solely of underscores, digits, and alphabetics
/// from the portable character set. The first character of a name is not a digit.
pub fn is_assignable_name(name: &str) -> bool {
    for (i, c) in name.chars().enumerate() {
        if !is_name_char(c) {
            return false;
        }
        if i == 0 && c >= '0' && c <= '9' {
            return false;
        }
    }
    return true;
}

pub fn parse_assignment_word(word: &str) -> Option<(&str, &str)> {
    match word.find('=') {
        Some(idx) => {
            let (key, val) = word.split_at(idx);
            if is_assignable_name(key) {
                Some((key, &val[1..]))
            } else {
                None
            }
        }
        _ => None,
    }
}

impl TokenKind {
    pub fn parse_assignment_word(&self) -> Option<(&str, &str)> {
        match self {
            TokenKind::Word(word) => parse_assignment_word(word),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: TokenPosition,
    pub end: TokenPosition,
}

impl Token {
    pub fn new(kind: TokenKind, start: TokenPosition, end: TokenPosition) -> Self {
        Self { kind, start, end }
    }

    pub fn apply_command_word_rules(&mut self, aliases: Option<&Aliases>) {
        // From Section 2.10.2 Shell Grammar rules, the command word has
        // some special processing.

        // Reserved word?
        if let TokenKind::Word(ref word) = self.kind {
            if let MatchResult::Match(reserved, _) = RESERVED_WORDS.matches(word) {
                self.kind = TokenKind::ReservedWord(reserved);
            }
        }

        // Perform alias expansion
        if let Some(aliases) = aliases {
            while let TokenKind::Word(ref word) = self.kind {
                if let Some(expanded) = aliases.lookup(&word) {
                    let recursive = expanded.ends_with(" ");
                    self.kind = TokenKind::Word(expanded.to_string());
                    if !recursive {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    pub fn is_reserved_word(&self, wanted: ReservedWord) -> bool {
        if let TokenKind::Word(ref word) = self.kind {
            if let MatchResult::Match(reserved, _) = RESERVED_WORDS.matches(word) {
                return reserved == wanted;
            }
        }
        false
    }
}

#[derive(Debug, Clone, Copy)]
struct PositionedChar {
    c: char,
    pos: TokenPosition,
}

enum Next {
    Char(PositionedChar),
    Eof,
    Error(Error),
}

impl<R: std::io::Read> Lexer<R> {
    pub fn new(source: &str, stream: R) -> Self {
        Self {
            stream: BufReader::new(stream),
            stream_buffer: String::new(),
            stream_buffer_pos: 0,
            source: source.to_owned(),
            stream_position: Default::default(),
            token_text: String::new(),
            tokens: vec![],
        }
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }

    fn next_char(&mut self) -> Next {
        if self.stream_buffer.is_empty() || self.stream_buffer_pos >= self.stream_buffer.len() {
            let bump_line = !self.stream_buffer.is_empty();
            self.stream_buffer.clear();
            match self.stream.read_line(&mut self.stream_buffer) {
                Ok(0) => return Next::Eof,
                Err(e) => return Next::Error(e.into()),
                _ => {
                    self.stream_buffer_pos = 0;
                    self.stream_position.col_number = 0;
                    if bump_line {
                        self.stream_position.line_number += 1;
                    }
                }
            }
        }
        match (&self.stream_buffer[self.stream_buffer_pos..])
            .chars()
            .next()
        {
            Some(c) => {
                let result = Next::Char(PositionedChar {
                    c,
                    pos: self.stream_position,
                });
                self.stream_buffer_pos += c.len_utf8();
                self.stream_position.col_number += 1;
                result
            }
            None => {
                self.stream_buffer_pos += 1;
                self.next_char()
            }
        }
    }

    fn unget(&mut self, c: PositionedChar) {
        let len = c.c.len_utf8();
        assert!(self.stream_buffer_pos > 0);
        assert!(len < self.stream_buffer_pos);
        self.stream_buffer_pos -= len;
        self.stream_position.col_number -= 1;
    }

    pub fn next(&mut self) -> Fallible<Token> {
        loop {
            let b = match self.next_char() {
                Next::Char(b) => b,
                Next::Eof => {
                    // Section 2.3.1
                    if self.token_text.is_empty() {
                        return Ok(Token::new(
                            TokenKind::Eof,
                            self.stream_position,
                            self.stream_position,
                        ));
                    }
                    return Ok(self.delimit_current());
                }
                Next::Error(e) => return Err(e),
            };

            self.accumulate(b);

            match OPERATORS.matches(&self.token_text) {
                MatchResult::Match(..) => {
                    // Section 2.3.2
                    continue;
                }
                MatchResult::No => {
                    // Section 2.3.3
                    // Together with the prior text, this most recent character doesn't
                    // form an operator, so look at the text without it; if that is one
                    // then we delimit a token
                    if self.token_text.len() > 1 {
                        if let MatchResult::Match(oper, len) =
                            OPERATORS.matches(&self.token_text[0..self.token_text.len() - 1])
                        {
                            let tok = Token::new(
                                TokenKind::Operator(oper),
                                self.tokens[0].pos,
                                self.tokens[len - 1].pos,
                            );
                            self.clear_tokens();
                            self.unget(b);
                            return Ok(tok);
                        }
                    }

                    // Section 2.3.6; if the latest character is the start
                    // of a new operator, then delim the current token
                    if !self.token_text.is_empty() && self.is_io_number(&self.token_text).is_none()
                    {
                        let len = self.token_text.len();
                        match OPERATORS.matches(&self.token_text[len - 1..len]) {
                            MatchResult::No => {}
                            _ => {
                                self.pop();
                                let tok = self.delimit_current();
                                self.unget(b);
                                return Ok(tok);
                            }
                        }
                    }
                }
            }

            match b.c {
                '\\' => {
                    let b = self.next_char_or_err(LexErrorKind::EofDuringBackslash)?;
                    if b.c == '\n' {
                        // Line continuation
                        self.pop();
                    } else {
                        // Else quoted character
                        self.accumulate(b);
                    }
                    continue;
                }
                '\'' => self.single_quoted()?,
                '"' => self.double_quoted()?,
                '$' => self.dollar()?,
                '`' => self.accumulate_quoted('`')?,
                '\n' => {
                    self.pop();
                    if !self.token_text.is_empty() {
                        let token = self.delimit_current();
                        self.unget(b);
                        return Ok(token);
                    }
                    let tok = Token::new(TokenKind::NewLine, b.pos, b.pos);
                    self.clear_tokens();
                    return Ok(tok);
                }
                ' ' | '\t' | '\r' => {
                    self.pop();
                    if self.token_text.is_empty() {
                        continue;
                    }
                    let tok = self.delimit_current();
                    return Ok(tok);
                }
                '#' => {
                    self.pop();
                    self.comment()?;
                }

                '>' | '<' => {
                    if let Some(number) = self.is_io_number(&self.token_text) {
                        let start = self.tokens[0].pos;
                        let end = self.tokens[number.matched_len - 1].pos;
                        self.clear_tokens();
                        self.unget(b);
                        return Ok(Token::new(TokenKind::IoNumber(number.fd_num), start, end));
                    }
                }

                _ => {}
            }
        }
    }

    fn is_io_number(&self, word: &str) -> Option<IoNumber> {
        let len = word.len();
        if len < 2 {
            return None;
        }
        let last = word.as_bytes()[len - 1];
        if last == b'<' || last == b'>' {
            let num_str = &word[0..len - 1];
            if let Ok(fd_num) = usize::from_str_radix(&num_str, 10) {
                return Some(IoNumber {
                    fd_num,
                    matched_len: len - 1,
                });
            }
        }
        None
    }

    fn delimit_current(&mut self) -> Token {
        let start = self.tokens.first().unwrap().pos;

        if let MatchResult::Match(oper, len) = OPERATORS.matches(&self.token_text) {
            if len == self.token_text.len() {
                let end = self.tokens[len - 1].pos;
                self.clear_tokens();
                return Token::new(TokenKind::Operator(oper), start, end);
            }
        }

        let word = std::mem::replace(&mut self.token_text, String::new());
        let end = self.tokens.last().unwrap().pos;
        self.clear_tokens();

        Token::new(TokenKind::Word(word), start, end)
    }

    fn comment(&mut self) -> Fallible<()> {
        loop {
            let b = self.next_char_or_err(LexErrorKind::EofDuringComment)?;

            if b.c == '\n' {
                return Ok(());
            }
        }
    }

    fn single_quoted(&mut self) -> Fallible<()> {
        let mut backslash = false;
        loop {
            let b = self.next_char_or_err(LexErrorKind::EofDuringSingleQuotedString)?;
            self.accumulate(b);

            if b.c == '\'' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b.c == '\\' {
                backslash = true;
            }
        }
    }

    fn double_quoted(&mut self) -> Fallible<()> {
        let mut backslash = false;
        loop {
            let b = self.next_char_or_err(LexErrorKind::EofDuringDoubleQuotedString)?;
            self.accumulate(b);

            if b.c == '"' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b.c == '\\' {
                backslash = true;
            }
        }
    }

    fn next_char_or_err(&mut self, err: LexErrorKind) -> Fallible<PositionedChar> {
        match self.next_char() {
            Next::Char(b) => Ok(b),
            Next::Eof => Err(err.at(self.tokens[0].pos, self.stream_position).into()),
            Next::Error(e) => Err(e.context(self.stream_position).into()),
        }
    }

    fn dollar(&mut self) -> Fallible<()> {
        let b = self.next_char_or_err(LexErrorKind::EofDuringParameterExpansion)?;
        self.accumulate(b);
        if b.c != '{' {
            return Ok(());
        }
        self.accumulate_quoted('}')
    }

    fn clear_tokens(&mut self) {
        self.tokens.clear();
        self.token_text.clear();
    }
    fn accumulate(&mut self, c: PositionedChar) {
        self.tokens.push(c);
        self.token_text.push(c.c);
    }
    fn pop(&mut self) {
        self.tokens.pop();
        self.token_text.pop();
    }

    fn accumulate_quoted(&mut self, expected: char) -> Fallible<()> {
        let mut close_stack = vec![expected];
        let mut backslash = false;
        loop {
            let b = self.next_char_or_err(LexErrorKind::EofDuringParameterExpansion)?;
            self.accumulate(b);

            if backslash {
                backslash = false;
                continue;
            }
            if b.c == '\\' {
                backslash = true;
                continue;
            }

            match b.c {
                '{' => close_stack.push('}'),
                '(' => close_stack.push(')'),
                '[' => close_stack.push(']'),
                _ => {
                    if *close_stack.last().unwrap() == b.c {
                        // Reached the end of this quoted section,
                        // so pop it off
                        close_stack.pop();
                        if close_stack.is_empty() {
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test_oper {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn oper() {
        assert_eq!(OPERATORS.matches("w"), MatchResult::No);
        assert_eq!(
            OPERATORS.matches("&"),
            MatchResult::Match(Operator::Ampersand, 1)
        );
        assert_eq!(
            OPERATORS.matches("&&"),
            MatchResult::Match(Operator::AndIf, 2)
        );
        assert_eq!(
            OPERATORS.matches("<"),
            MatchResult::Match(Operator::Less, 1)
        );
        assert_eq!(
            OPERATORS.matches("<<"),
            MatchResult::Match(Operator::DoubleLess, 2)
        );
        assert_eq!(
            OPERATORS.matches("<<-"),
            MatchResult::Match(Operator::DoubleLessDash, 3)
        );
    }
}

#[cfg(test)]
mod test_lex {
    use super::*;
    use pretty_assertions::assert_eq;

    fn lex(text: &str) -> (Vec<Token>, Option<String>) {
        let mut lexer = Lexer::new("test", text.as_bytes());
        let mut tokens = vec![];
        loop {
            match lexer.next() {
                Ok(Token {
                    kind: TokenKind::Eof,
                    ..
                }) => return (tokens, None),
                Ok(tok) => tokens.push(tok),
                Err(err) => return (tokens, Some(format!("{}", err))),
            }
        }
    }

    #[test]
    fn empty() {
        assert_eq!(lex(""), (vec![], None));
    }

    #[test]
    fn comment() {
        assert_eq!(
            lex("(# woot\n)"),
            (
                vec![
                    Token {
                        kind: TokenKind::Operator(Operator::LeftParen),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::RightParen),
                        start: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        }
                    },
                ],
                None
            )
        );
    }

    #[test]
    fn basics() {
        assert_eq!(
            lex("()\na"),
            (
                vec![
                    Token {
                        kind: TokenKind::Operator(Operator::LeftParen),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::RightParen),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 1
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 1
                        }
                    },
                    Token {
                        kind: TokenKind::NewLine,
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 2
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 2
                        }
                    },
                    Token {
                        kind: TokenKind::Word("a".to_string()),
                        start: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        }
                    },
                ],
                None
            )
        );
    }

    #[test]
    fn and_if() {
        assert_eq!(
            lex("true && false"),
            (
                vec![
                    Token {
                        kind: TokenKind::Word("true".to_string()),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 3
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::AndIf),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 5
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 6
                        }
                    },
                    Token {
                        kind: TokenKind::Word("false".to_string()),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 8
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 12
                        }
                    },
                ],
                None
            )
        );
        assert_eq!(
            lex("true&&false"),
            (
                vec![
                    Token {
                        kind: TokenKind::Word("true".to_string()),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 3
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::AndIf),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 4
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 5
                        }
                    },
                    Token {
                        kind: TokenKind::Word("false".to_string()),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 6
                        },
                        end: TokenPosition {
                            line_number: 0,
                            col_number: 10
                        }
                    },
                ],
                None
            )
        );
    }

    #[test]
    fn single_quote() {
        assert_eq!(
            lex("'hello'"),
            (
                vec![Token {
                    kind: TokenKind::Word("'hello'".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 6
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("'hel\\'lo'"),
            (
                vec![Token {
                    kind: TokenKind::Word("'hel\\'lo'".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 8
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("hello'world'"),
            (
                vec![Token {
                    kind: TokenKind::Word("hello'world'".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 11
                    }
                }],
                None
            )
        );
    }

    #[test]
    fn double_quote() {
        assert_eq!(
            lex("\"hello\""),
            (
                vec![Token {
                    kind: TokenKind::Word("\"hello\"".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 6
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("\"hel\\'lo\""),
            (
                vec![Token {
                    kind: TokenKind::Word("\"hel\\'lo\"".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 8
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("hello\"world\""),
            (
                vec![Token {
                    kind: TokenKind::Word("hello\"world\"".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 11
                    }
                }],
                None
            )
        );
    }

    #[test]
    fn backslash() {
        assert_eq!(
            lex("\\"),
            (
                vec![],
                Some("EOF while lexing backslash escape starting at line 0 column 0 ending at line 0 column 1".to_owned())
            )
        );
        assert_eq!(
            lex("\\\\"),
            (
                vec![Token {
                    kind: TokenKind::Word("\\\\".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 1
                    }
                },],
                None
            )
        );
        assert_eq!(
            lex("a\\\na b"),
            (
                vec![
                    Token {
                        kind: TokenKind::Word("aa".to_string()),
                        start: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        end: TokenPosition {
                            line_number: 1,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Word("b".to_string()),
                        start: TokenPosition {
                            line_number: 1,
                            col_number: 2
                        },
                        end: TokenPosition {
                            line_number: 1,
                            col_number: 2
                        }
                    },
                ],
                None
            )
        );
    }

    #[test]
    fn command_word() {
        let (mut tokens, err) = lex("echo hello");
        assert_eq!(err, None);

        tokens[0].apply_command_word_rules(None);
        assert_eq!(
            tokens[0],
            Token {
                kind: TokenKind::Word("echo".to_string()),
                start: TokenPosition {
                    line_number: 0,
                    col_number: 0
                },
                end: TokenPosition {
                    line_number: 0,
                    col_number: 3
                }
            }
        );

        let (mut tokens, err) = lex("if true");
        assert_eq!(err, None);

        tokens[0].apply_command_word_rules(None);
        assert_eq!(
            tokens[0],
            Token {
                kind: TokenKind::ReservedWord(ReservedWord::If),
                start: TokenPosition {
                    line_number: 0,
                    col_number: 0
                },
                end: TokenPosition {
                    line_number: 0,
                    col_number: 1
                }
            }
        );

        let mut aliases = Aliases::new();
        aliases.alias("ls", "ls -l");

        let (mut tokens, err) = lex("ls");
        assert_eq!(err, None);

        tokens[0].apply_command_word_rules(Some(&aliases));
        assert_eq!(
            tokens[0],
            Token {
                kind: TokenKind::Word("ls -l".to_string()),
                start: TokenPosition {
                    line_number: 0,
                    col_number: 0
                },
                end: TokenPosition {
                    line_number: 0,
                    col_number: 1
                }
            }
        );

        let (mut tokens, err) = lex("echo");
        assert_eq!(err, None);
        tokens[0].apply_command_word_rules(Some(&aliases));
        assert_eq!(
            tokens[0],
            Token {
                kind: TokenKind::Word("echo".to_string()),
                start: TokenPosition {
                    line_number: 0,
                    col_number: 0
                },
                end: TokenPosition {
                    line_number: 0,
                    col_number: 3
                }
            }
        );
    }

    #[test]
    fn io_redirect() {
        assert_eq!(
            lex("1<"),
            (
                vec![
                    Token::new(
                        TokenKind::IoNumber(1),
                        TokenPosition {
                            line_number: 0,
                            col_number: 0
                        },
                        TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    ),
                    Token::new(
                        TokenKind::Operator(Operator::Less),
                        TokenPosition {
                            line_number: 0,
                            col_number: 1
                        },
                        TokenPosition {
                            line_number: 0,
                            col_number: 1
                        }
                    ),
                ],
                None
            )
        );
    }

    #[test]
    fn name() {
        assert!(is_assignable_name("foo"));
        assert!(!is_assignable_name("1foo"));
        assert!(is_assignable_name("foo1"));
        assert!(is_assignable_name("foo_1"));
        assert!(is_assignable_name("_foo_1"));
    }

    #[test]
    fn assignment_word() {
        let tok = TokenKind::Word("foo".to_string());
        assert_eq!(tok.parse_assignment_word(), None);

        let tok = TokenKind::Word("foo=bar".to_string());
        assert_eq!(tok.parse_assignment_word(), Some(("foo", "bar")));

        let tok = TokenKind::Word("foo=bar=baz".to_string());
        assert_eq!(tok.parse_assignment_word(), Some(("foo", "bar=baz")));
    }

    #[test]
    fn dollar() {
        assert_eq!(
            lex("foo$hello"),
            (
                vec![Token {
                    kind: TokenKind::Word("foo$hello".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 8
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("${hello}there"),
            (
                vec![Token {
                    kind: TokenKind::Word("${hello}there".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 12
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("${he||o}there"),
            (
                vec![Token {
                    kind: TokenKind::Word("${he||o}there".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 12
                    }
                }],
                None
            )
        );
        assert_eq!(
            lex("${{e||}there"),
            (
                vec![],
                Some("EOF while lexing parameter expansion starting at line 0 column 0 ending at line 0 column 12".to_string()),
            )
        );
        assert_eq!(
            lex("${e||}}there"),
            (
                vec![Token {
                    kind: TokenKind::Word("${e||}}there".to_string()),
                    start: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    },
                    end: TokenPosition {
                        line_number: 0,
                        col_number: 11
                    }
                }],
                None
            )
        );
    }
}
