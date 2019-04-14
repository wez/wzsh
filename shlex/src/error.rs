use failure::Fail;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPosition {
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for TokenPosition {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "at line {} column {}", self.line, self.col)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TokenPositionWithWithSource {
    pub position: TokenPosition,
    pub source: String,
}

impl std::fmt::Display for TokenPositionWithWithSource {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "in {} {}", self.source, self.position)
    }
}

#[derive(Debug, Clone, Copy, Fail)]
pub enum LexErrorKind {
    #[fail(display = "EOF while lexing backslash escape")]
    EofDuringBackslash,
    #[fail(display = "EOF while lexing comment")]
    EofDuringComment,
    #[fail(display = "EOF while lexing single quoted string")]
    EofDuringSingleQuotedString,
    #[fail(display = "EOF while lexing double quoted string")]
    EofDuringDoubleQuotedString,
    #[fail(display = "EOF while lexing parameter expansion")]
    EofDuringParameterExpansion,
    #[fail(display = "IO Error")]
    IoError,
}

impl LexErrorKind {
    pub fn at(self, start: TokenPosition, end: TokenPosition) -> LexError {
        LexError::new(self, start, end)
    }
}

#[derive(Debug, Clone, Fail)]
#[fail(display = "{} starting {} ending {}", kind, start, end)]
pub struct LexError {
    kind: LexErrorKind,
    start: TokenPosition,
    end: TokenPosition,
}

impl LexError {
    pub fn new(kind: LexErrorKind, start: TokenPosition, end: TokenPosition) -> Self {
        Self { kind, start, end }
    }
}
