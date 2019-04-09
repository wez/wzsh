//! Shell lexer
//! See https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html

use lazy_static::lazy_static;
use std::io::{BufRead, BufReader};

pub mod alias;
pub mod environment;
pub mod expander;
pub mod string;
pub use alias::Aliases;
pub use environment::Environment;
pub use expander::Expander;

#[derive(Debug)]
pub struct Error {
    pub position: TokenPosition,
    pub err: std::io::Error,
}

impl Error {
    pub fn from_io(err: std::io::Error, position: TokenPosition) -> Self {
        Self { err, position }
    }

    pub fn with_message(message: &str, position: TokenPosition) -> Self {
        Self {
            err: std::io::Error::new(std::io::ErrorKind::Other, message),
            position,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{} at line {} column {}",
            self.err, self.position.line_number, self.position.col_number
        )
    }
}

pub struct Lexer<R: std::io::Read> {
    source: String,
    stream: BufReader<R>,
    position: TokenPosition,
    token_text: String,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPosition {
    pub line_number: usize,
    pub col_number: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Operator {
    /// `&&`
    AndIf,
    /// `||`
    OrIf,
    /// `;;`
    DoubleSemicolon,
    /// `<<`
    DoubleLess,
    /// `>>`
    DoubleGreat,
    /// `<&`
    LessAnd,
    /// `>&`
    GreatAnd,
    /// `<>`
    LessGreat,
    /// `<<-`
    DoubleLessDash,
    /// `>|`
    Clobber,
    /// `|`
    Pipe,
    /// `&`
    Ampersand,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `<`
    Less,
    /// `>`
    Great,
    /// `;`
    Semicolon,
}

#[derive(Debug)]
struct MatchEntry<T: Copy> {
    text: &'static str,
    value: T,
    ambiguous_unless_len_is: usize,
}

#[derive(Debug)]
struct LiteralMatcher<T: Copy> {
    literals: Vec<MatchEntry<T>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MatchResult<T: Copy> {
    Match(T, usize),
    Ambiguous(usize),
    No,
}

impl<T: Copy> LiteralMatcher<T> {
    fn new(literals: &[(&'static str, T)]) -> Self {
        let mut literals: Vec<MatchEntry<T>> = literals
            .iter()
            .map(|(text, value)| MatchEntry {
                text,
                value: *value,
                ambiguous_unless_len_is: 0,
            })
            .collect();

        // Sort by length of token descending
        literals.sort_by(|a, b| b.text.len().cmp(&a.text.len()));

        // Compute disambiguation information
        let num_literals = literals.len();
        for i in 0..num_literals {
            let lit = &literals[i].text;
            let mut longest = lit.len();
            for j in 0..num_literals {
                if i == j {
                    continue;
                }

                if literals[j].text.starts_with(lit) {
                    let len = literals[j].text.len();
                    longest = longest.max(len);
                }
            }

            literals[i].ambiguous_unless_len_is = longest;
        }

        Self { literals }
    }

    fn matches(&self, text: &str, is_final: bool) -> MatchResult<T> {
        for entry in &self.literals {
            let len = entry.text.len().min(text.len());
            if &text[0..len] == &entry.text[0..len] {
                if entry.ambiguous_unless_len_is == len {
                    return MatchResult::Match(entry.value, entry.text.len());
                }
                if !is_final {
                    return MatchResult::Ambiguous(entry.ambiguous_unless_len_is);
                } else if entry.text.len() <= text.len() {
                    return MatchResult::Match(entry.value, entry.text.len());
                }
            }
        }
        MatchResult::No
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReservedWord {
    If,
    Then,
    Else,
    Elif,
    Fi,
    Do,
    Done,
    Case,
    Esac,
    While,
    Until,
    For,
    LeftBrace,
    RightBrace,
    Bang,
    In,
}

lazy_static! {
    static ref OPERATORS: LiteralMatcher<Operator> = {
        LiteralMatcher::new(&[
            ("<<-", Operator::DoubleLessDash),
            ("<<", Operator::DoubleLess),
            ("<&", Operator::LessAnd),
            ("<>", Operator::LessGreat),
            (">>", Operator::DoubleGreat),
            (">|", Operator::Clobber),
            (">&", Operator::GreatAnd),
            ("&&", Operator::AndIf),
            ("||", Operator::OrIf),
            (";;", Operator::Semicolon),
            ("<", Operator::Less),
            ("&", Operator::Ampersand),
            ("|", Operator::Pipe),
            (";", Operator::Semicolon),
            (">", Operator::Great),
            ("(", Operator::LeftParen),
            (")", Operator::RightParen),
        ])
    };
    static ref RESERVED_WORDS: LiteralMatcher<ReservedWord> = {
        LiteralMatcher::new(&[
            ("if", ReservedWord::If),
            ("then", ReservedWord::Then),
            ("else", ReservedWord::Else),
            ("elif", ReservedWord::Elif),
            ("fi", ReservedWord::Fi),
            ("do", ReservedWord::Do),
            ("done", ReservedWord::Done),
            ("case", ReservedWord::Case),
            ("esac", ReservedWord::Esac),
            ("while", ReservedWord::While),
            ("until", ReservedWord::Until),
            ("for", ReservedWord::For),
            ("{", ReservedWord::LeftBrace),
            ("}", ReservedWord::RightBrace),
            ("!", ReservedWord::Bang),
            ("in", ReservedWord::In),
        ])
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IoNumber {
    /// `1<` -> Input(1)
    Input(usize),
    /// `2>` -> Output(2)
    Output(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Eof,
    Operator(Operator),
    ReservedWord(ReservedWord),
    Name(String),
    Word(String),
    IoNumber(IoNumber),
    NewLine,
}

/// In the shell command language, a word consisting solely of underscores, digits, and alphabetics
/// from the portable character set. The first character of a name is not a digit.
pub fn is_name(name: &str) -> bool {
    for (i, c) in name.chars().enumerate() {
        if c >= '0' && c <= '9' {
            if i == 0 {
                return false;
            }
            continue;
        }
        if c >= 'a' && c <= 'z' {
            continue;
        }
        if c >= 'A' && c <= 'Z' {
            continue;
        }
        if c == '_' {
            continue;
        }
        return false;
    }
    return true;
}

pub fn parse_assignment_word(word: &str) -> Option<(&str, &str)> {
    match word.find('=') {
        Some(idx) => {
            let (key, val) = word.split_at(idx);
            if is_name(key) {
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
    pub position: TokenPosition,
}

impl Token {
    pub fn new(kind: TokenKind, position: TokenPosition) -> Self {
        Self { kind, position }
    }

    pub fn apply_command_word_rules(&mut self, aliases: Option<&Aliases>) {
        // From Section 2.10.2 Shell Grammar rules, the command word has
        // some special processing.

        // Reserved word?
        if let TokenKind::Word(ref word) = self.kind {
            if let MatchResult::Match(reserved, _) = RESERVED_WORDS.matches(word, true) {
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
                }
            }
        }
    }
}

enum Next {
    Char(char),
    Eof,
    Error(std::io::Error),
}

impl<R: std::io::Read> Lexer<R> {
    pub fn new(source: &str, stream: R) -> Self {
        Self {
            stream: BufReader::new(stream),
            source: source.to_owned(),
            position: Default::default(),
            token_text: String::new(),
        }
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }

    fn next_char(&mut self) -> Next {
        match self.stream.fill_buf() {
            Ok(buf) => {
                if buf.is_empty() {
                    Next::Eof
                } else {
                    let len = buf.len().min(4);
                    let buf = &buf[0..len];
                    match std::str::from_utf8(buf) {
                        Ok(s) => {
                            let c = s.chars().next().unwrap();
                            self.stream.consume(c.len_utf8());
                            Next::Char(c)
                        }
                        Err(e) => {
                            let (good, _) = buf.split_at(e.valid_up_to());
                            if !good.is_empty() {
                                let c = std::str::from_utf8(good).unwrap().chars().next().unwrap();
                                self.stream.consume(c.len_utf8());
                                Next::Char(c)
                            } else {
                                // Presumably need more data
                                Next::Error(std::io::Error::new(
                                    std::io::ErrorKind::Other,
                                    format!("unable to decode utf8 from {:?}", buf),
                                ))
                            }
                        }
                    }
                }
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    Next::Eof
                } else {
                    Next::Error(e)
                }
            }
        }
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        loop {
            if self.token_text == "\n" {
                let pos = self.position;
                let tok = Token::new(TokenKind::NewLine, pos);
                self.position.col_number = 0;
                self.position.line_number += 1;
                self.token_text.clear();
                return Ok(tok);
            }

            let b = match self.next_char() {
                Next::Char(b) => b,
                Next::Eof => {
                    // Section 2.3.1
                    if self.token_text.is_empty() {
                        return Ok(Token::new(TokenKind::Eof, self.position));
                    }
                    return Ok(self.delimit_current());
                }
                Next::Error(e) => return Err(Error::from_io(e, self.position)),
            };

            self.token_text.push(b);

            // Section 2.3.2 + 2.3.3
            match OPERATORS.matches(&self.token_text, false) {
                MatchResult::Match(oper, len) => {
                    let pos = self.position;
                    self.position.col_number += len;
                    for _ in 0..len {
                        self.token_text.remove(0);
                    }
                    return Ok(Token::new(TokenKind::Operator(oper), pos));
                }
                MatchResult::Ambiguous(_) => {}
                MatchResult::No => {
                    // Section 2.3.6; if the latest character is the start
                    // of a new operator, then delim the current token
                    if !self.token_text.is_empty() && self.is_io_number(&self.token_text).is_none()
                    {
                        let len = self.token_text.len();
                        match OPERATORS.matches(&self.token_text[len - 1..len], false) {
                            MatchResult::No => {}
                            _ => {
                                self.token_text.pop();
                                let tok = self.delimit_current();
                                self.token_text.push(b);
                                return Ok(tok);
                            }
                        }
                    }
                }
            }

            match b {
                '\\' => {
                    let b = match self.next_char() {
                        Next::Char(b) => b,
                        Next::Eof => {
                            return Err(Error::with_message(
                                "unexpected end of file while lexing backslash",
                                self.position,
                            ));
                        }
                        Next::Error(e) => return Err(Error::from_io(e, self.position)),
                    };
                    if b == '\n' {
                        // Line continuation
                        self.token_text.pop();
                    } else {
                        // Else quoted character
                        self.token_text.push(b);
                    }
                    continue;
                }
                '\'' => self.single_quoted()?,
                '"' => self.double_quoted()?,
                // TODO: $ and backtick
                '\n' => {
                    self.token_text.pop();
                    if !self.token_text.is_empty() {
                        let token = self.delimit_current();
                        self.token_text.push(b);
                        return Ok(token);
                    }
                    self.token_text.push(b);
                    continue;
                }
                ' ' | '\t' | '\r' => {
                    self.token_text.pop();
                    if self.token_text.is_empty() {
                        self.position.col_number += 1;
                        continue;
                    }
                    let tok = self.delimit_current();
                    self.position.col_number += 1;
                    return Ok(tok);
                }
                '#' => {
                    self.token_text.pop();
                    self.comment()?;
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
            if let Ok(num) = usize::from_str_radix(&num_str, 10) {
                let number = if last == b'<' {
                    IoNumber::Input(num)
                } else {
                    IoNumber::Output(num)
                };
                return Some(number);
            }
        }
        None
    }

    fn delimit_current(&mut self) -> Token {
        if let MatchResult::Match(oper, len) = OPERATORS.matches(&self.token_text, true) {
            if len == self.token_text.len() {
                let pos = self.position;
                self.position.col_number += len;
                self.token_text.clear();
                return Token::new(TokenKind::Operator(oper), pos);
            }
        }

        let word = std::mem::replace(&mut self.token_text, String::new());
        let pos = self.position;
        self.position.col_number += word.len();

        if let Some(number) = self.is_io_number(&word) {
            return Token::new(TokenKind::IoNumber(number), pos);
        }

        Token::new(TokenKind::Word(word), pos)
    }

    fn comment(&mut self) -> Result<(), Error> {
        loop {
            let b = match self.next_char() {
                Next::Char(b) => b,
                Next::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing comment",
                        self.position,
                    ));
                }
                Next::Error(e) => return Err(Error::from_io(e, self.position)),
            };

            if b == '\n' {
                self.position.col_number = 0;
                self.position.line_number += 1;
                return Ok(());
            }
        }
    }

    fn single_quoted(&mut self) -> Result<(), Error> {
        let mut backslash = false;
        loop {
            let b = match self.next_char() {
                Next::Char(b) => b,
                Next::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing single quoted string",
                        self.position,
                    ));
                }
                Next::Error(e) => return Err(Error::from_io(e, self.position)),
            };
            self.token_text.push(b);

            if b == '\'' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b == '\\' {
                backslash = true;
            }
        }
    }

    fn double_quoted(&mut self) -> Result<(), Error> {
        let mut backslash = false;
        loop {
            let b = match self.next_char() {
                Next::Char(b) => b,
                Next::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing double quoted string",
                        self.position,
                    ));
                }
                Next::Error(e) => return Err(Error::from_io(e, self.position)),
            };
            self.token_text.push(b);

            if b == '"' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b == '\\' {
                backslash = true;
            }
        }
    }
}

#[cfg(test)]
mod test_oper {
    use super::*;

    #[test]
    fn oper() {
        assert_eq!(OPERATORS.matches("w", false), MatchResult::No);
        assert_eq!(OPERATORS.matches("w", true), MatchResult::No);
        assert_eq!(OPERATORS.matches("&", false), MatchResult::Ambiguous(2));
        assert_eq!(
            OPERATORS.matches("&", true),
            MatchResult::Match(Operator::Ampersand, 1)
        );
        assert_eq!(
            OPERATORS.matches("&&", false),
            MatchResult::Match(Operator::AndIf, 2)
        );
        assert_eq!(
            OPERATORS.matches("&&", true),
            MatchResult::Match(Operator::AndIf, 2)
        );
        assert_eq!(OPERATORS.matches("<", false), MatchResult::Ambiguous(3));
        assert_eq!(OPERATORS.matches("<<", false), MatchResult::Ambiguous(3));
        assert_eq!(
            OPERATORS.matches("<<", true),
            MatchResult::Match(Operator::DoubleLess, 2)
        );
        assert_eq!(
            OPERATORS.matches("<<-", false),
            MatchResult::Match(Operator::DoubleLessDash, 3)
        );
    }
}

#[cfg(test)]
mod test_lex {
    use super::*;

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
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::RightParen),
                        position: TokenPosition {
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
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::RightParen),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 1
                        }
                    },
                    Token {
                        kind: TokenKind::NewLine,
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 2
                        }
                    },
                    Token {
                        kind: TokenKind::Word("a".to_string()),
                        position: TokenPosition {
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
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::AndIf),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 5
                        }
                    },
                    Token {
                        kind: TokenKind::Word("false".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 8
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
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 0
                        }
                    },
                    Token {
                        kind: TokenKind::Operator(Operator::AndIf),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 4
                        }
                    },
                    Token {
                        kind: TokenKind::Word("false".to_string()),
                        position: TokenPosition {
                            line_number: 0,
                            col_number: 6
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
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
                Some("unexpected end of file while lexing backslash at line 0 column 0".to_owned())
            )
        );
        assert_eq!(
            lex("\\\\"),
            (
                vec![Token {
                    kind: TokenKind::Word("\\\\".to_string()),
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    }
                },],
                None
            )
        );
        assert_eq!(
            lex("\\\na"),
            (
                vec![Token {
                    kind: TokenKind::Word("a".to_string()),
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    }
                },],
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
                position: TokenPosition {
                    line_number: 0,
                    col_number: 0
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
                position: TokenPosition {
                    line_number: 0,
                    col_number: 0
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
                position: TokenPosition {
                    line_number: 0,
                    col_number: 0
                }
            }
        );
    }

    #[test]
    fn io_redirect() {
        assert_eq!(
            lex("1<"),
            (
                vec![Token {
                    kind: TokenKind::IoNumber(IoNumber::Input(1)),
                    position: TokenPosition {
                        line_number: 0,
                        col_number: 0
                    }
                },],
                None
            )
        );
    }

    #[test]
    fn name() {
        assert!(is_name("foo"));
        assert!(!is_name("1foo"));
        assert!(is_name("foo1"));
        assert!(is_name("foo_1"));
        assert!(is_name("_foo_1"));
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
}
