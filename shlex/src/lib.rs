//! Shell lexer
//! See https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html

use lazy_static::lazy_static;

pub mod alias;
pub mod environment;
pub use alias::Aliases;
pub use environment::Environment;

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
    stream: R,
    position: TokenPosition,
    token_text: Vec<u8>,
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
    text: &'static [u8],
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
    fn new(literals: &[(&'static [u8], T)]) -> Self {
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

    fn matches(&self, text: &[u8], is_final: bool) -> MatchResult<T> {
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
            (b"<<-", Operator::DoubleLessDash),
            (b"<<", Operator::DoubleLess),
            (b"<&", Operator::LessAnd),
            (b"<>", Operator::LessGreat),
            (b">>", Operator::DoubleGreat),
            (b">|", Operator::Clobber),
            (b">&", Operator::GreatAnd),
            (b"&&", Operator::AndIf),
            (b"||", Operator::OrIf),
            (b";;", Operator::Semicolon),
            (b"<", Operator::Less),
            (b"&", Operator::Ampersand),
            (b"|", Operator::Pipe),
            (b";", Operator::Semicolon),
            (b">", Operator::Great),
            (b"(", Operator::LeftParen),
            (b")", Operator::RightParen),
        ])
    };
    static ref RESERVED_WORDS: LiteralMatcher<ReservedWord> = {
        LiteralMatcher::new(&[
            (b"if", ReservedWord::If),
            (b"then", ReservedWord::Then),
            (b"else", ReservedWord::Else),
            (b"elif", ReservedWord::Elif),
            (b"fi", ReservedWord::Fi),
            (b"do", ReservedWord::Do),
            (b"done", ReservedWord::Done),
            (b"case", ReservedWord::Case),
            (b"esac", ReservedWord::Esac),
            (b"while", ReservedWord::While),
            (b"until", ReservedWord::Until),
            (b"for", ReservedWord::For),
            (b"{", ReservedWord::LeftBrace),
            (b"}", ReservedWord::RightBrace),
            (b"!", ReservedWord::Bang),
            (b"in", ReservedWord::In),
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
    Name(Vec<u8>),
    Word(Vec<u8>),
    IoNumber(IoNumber),
    NewLine,
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
                    let recursive = expanded.ends_with(b" ");
                    self.kind = TokenKind::Word(expanded.to_vec());
                    if !recursive {
                        break;
                    }
                }
            }
        }
    }
}

enum NextByte {
    Byte(u8),
    Eof,
    Error(std::io::Error),
}

impl<R: std::io::Read> Lexer<R> {
    pub fn new(source: &str, stream: R) -> Self {
        Self {
            stream,
            source: source.to_owned(),
            position: Default::default(),
            token_text: vec![],
        }
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }

    fn next_byte(&mut self) -> NextByte {
        let mut buf = [0u8; 1];
        match self.stream.read(&mut buf) {
            Ok(1) => NextByte::Byte(buf[0]),
            Ok(_) => NextByte::Eof,
            Err(e) => {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    NextByte::Eof
                } else {
                    NextByte::Error(e)
                }
            }
        }
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        loop {
            if self.token_text == b"\n" {
                let pos = self.position;
                let tok = Token::new(TokenKind::NewLine, pos);
                self.position.col_number = 0;
                self.position.line_number += 1;
                self.token_text.clear();
                return Ok(tok);
            }

            let b = match self.next_byte() {
                NextByte::Byte(b) => b,
                NextByte::Eof => {
                    // Section 2.3.1
                    if self.token_text.is_empty() {
                        return Ok(Token::new(TokenKind::Eof, self.position));
                    }
                    return Ok(self.delimit_current());
                }
                NextByte::Error(e) => return Err(Error::from_io(e, self.position)),
            };

            self.token_text.push(b);

            // Section 2.3.2 + 2.3.3
            match OPERATORS.matches(self.token_text.as_slice(), false) {
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
                        match OPERATORS.matches(&[b], false) {
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
                b'\\' => {
                    let b = match self.next_byte() {
                        NextByte::Byte(b) => b,
                        NextByte::Eof => {
                            return Err(Error::with_message(
                                "unexpected end of file while lexing backslash",
                                self.position,
                            ));
                        }
                        NextByte::Error(e) => return Err(Error::from_io(e, self.position)),
                    };
                    if b == b'\n' {
                        // Line continuation
                        self.token_text.pop();
                    } else {
                        // Else quoted character
                        self.token_text.push(b);
                    }
                    continue;
                }
                b'\'' => self.single_quoted()?,
                b'"' => self.double_quoted()?,
                // TODO: $ and backtick
                b'\n' => {
                    self.token_text.pop();
                    if !self.token_text.is_empty() {
                        let token = self.delimit_current();
                        self.token_text.push(b);
                        return Ok(token);
                    }
                    self.token_text.push(b);
                    continue;
                }
                b' ' | b'\t' | b'\r' => {
                    self.token_text.pop();
                    if self.token_text.is_empty() {
                        self.position.col_number += 1;
                        continue;
                    }
                    let tok = self.delimit_current();
                    self.position.col_number += 1;
                    return Ok(tok);
                }
                b'#' => {
                    self.token_text.pop();
                    self.comment()?;
                }
                _ => {}
            }
        }
    }

    fn is_io_number(&self, word: &[u8]) -> Option<IoNumber> {
        let len = word.len();
        if len < 2 {
            return None;
        }
        let last = word[len - 1];
        if last == b'<' || last == b'>' {
            let num_str = String::from_utf8_lossy(&word[0..len - 1]);
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
        if let MatchResult::Match(oper, len) = OPERATORS.matches(self.token_text.as_slice(), true) {
            if len == self.token_text.len() {
                let pos = self.position;
                self.position.col_number += len;
                self.token_text.clear();
                return Token::new(TokenKind::Operator(oper), pos);
            }
        }

        let word = std::mem::replace(&mut self.token_text, vec![]);
        let pos = self.position;
        self.position.col_number += word.len();

        if let Some(number) = self.is_io_number(&word) {
            return Token::new(TokenKind::IoNumber(number), pos);
        }

        Token::new(TokenKind::Word(word), pos)
    }

    fn comment(&mut self) -> Result<(), Error> {
        loop {
            let b = match self.next_byte() {
                NextByte::Byte(b) => b,
                NextByte::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing comment",
                        self.position,
                    ));
                }
                NextByte::Error(e) => return Err(Error::from_io(e, self.position)),
            };

            if b == b'\n' {
                self.position.col_number = 0;
                self.position.line_number += 1;
                return Ok(());
            }
        }
    }

    fn single_quoted(&mut self) -> Result<(), Error> {
        let mut backslash = false;
        loop {
            let b = match self.next_byte() {
                NextByte::Byte(b) => b,
                NextByte::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing single quoted string",
                        self.position,
                    ));
                }
                NextByte::Error(e) => return Err(Error::from_io(e, self.position)),
            };
            self.token_text.push(b);

            if b == b'\'' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b == b'\\' {
                backslash = true;
            }
        }
    }

    fn double_quoted(&mut self) -> Result<(), Error> {
        let mut backslash = false;
        loop {
            let b = match self.next_byte() {
                NextByte::Byte(b) => b,
                NextByte::Eof => {
                    return Err(Error::with_message(
                        "unexpected end of file while lexing double quoted string",
                        self.position,
                    ));
                }
                NextByte::Error(e) => return Err(Error::from_io(e, self.position)),
            };
            self.token_text.push(b);

            if b == b'"' && !backslash {
                return Ok(());
            }

            backslash = false;

            if b == b'\\' {
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
        assert_eq!(OPERATORS.matches(b"w", false), MatchResult::No);
        assert_eq!(OPERATORS.matches(b"w", true), MatchResult::No);
        assert_eq!(OPERATORS.matches(b"&", false), MatchResult::Ambiguous(2));
        assert_eq!(
            OPERATORS.matches(b"&", true),
            MatchResult::Match(Operator::Ampersand, 1)
        );
        assert_eq!(
            OPERATORS.matches(b"&&", false),
            MatchResult::Match(Operator::AndIf, 2)
        );
        assert_eq!(
            OPERATORS.matches(b"&&", true),
            MatchResult::Match(Operator::AndIf, 2)
        );
        assert_eq!(OPERATORS.matches(b"<", false), MatchResult::Ambiguous(3));
        assert_eq!(OPERATORS.matches(b"<<", false), MatchResult::Ambiguous(3));
        assert_eq!(
            OPERATORS.matches(b"<<", true),
            MatchResult::Match(Operator::DoubleLess, 2)
        );
        assert_eq!(
            OPERATORS.matches(b"<<-", false),
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
                        kind: TokenKind::Word(vec![b'a']),
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
                        kind: TokenKind::Word(b"true".to_vec()),
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
                        kind: TokenKind::Word(b"false".to_vec()),
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
                        kind: TokenKind::Word(b"true".to_vec()),
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
                        kind: TokenKind::Word(b"false".to_vec()),
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
                    kind: TokenKind::Word(b"'hello'".to_vec()),
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
                    kind: TokenKind::Word(b"'hel\\'lo'".to_vec()),
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
                    kind: TokenKind::Word(b"hello'world'".to_vec()),
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
                    kind: TokenKind::Word(b"\"hello\"".to_vec()),
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
                    kind: TokenKind::Word(b"\"hel\\'lo\"".to_vec()),
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
                    kind: TokenKind::Word(b"hello\"world\"".to_vec()),
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
                    kind: TokenKind::Word(b"\\\\".to_vec()),
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
                    kind: TokenKind::Word(b"a".to_vec()),
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
                kind: TokenKind::Word(b"echo".to_vec()),
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
        aliases.alias(b"ls", b"ls -l");

        let (mut tokens, err) = lex("ls");
        assert_eq!(err, None);

        tokens[0].apply_command_word_rules(Some(&aliases));
        assert_eq!(
            tokens[0],
            Token {
                kind: TokenKind::Word(b"ls -l".to_vec()),
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
}
