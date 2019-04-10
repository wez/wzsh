use failure::Fail;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPosition {
    pub line_number: usize,
    pub col_number: usize,
}

impl std::fmt::Display for TokenPosition {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "at line {} column {}",
            self.line_number, self.col_number
        )
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
    #[fail(display = "Invalid UTF-8 sequence while reading program text")]
    InvalidUtf8InProgramText,
    #[fail(display = "IO Error")]
    IoError,
}

impl LexErrorKind {
    pub fn at(self, position: TokenPosition) -> LexError {
        LexError::new(self, position)
    }
}

#[derive(Debug, Clone, Fail)]
#[fail(display = "{} {}", kind, position)]
pub struct LexError {
    kind: LexErrorKind,
    position: TokenPosition,
}

impl LexError {
    pub fn new(kind: LexErrorKind, position: TokenPosition) -> Self {
        Self { kind, position }
    }
}
