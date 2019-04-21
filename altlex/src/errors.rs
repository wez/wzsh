use crate::position::Span;
use failure::Fail;

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
    #[fail(display = "EOF while lexing assignment word")]
    EofDuringAssignmentWord,
    #[fail(display = "EOF while lexing command substitution")]
    EofDuringCommandSubstitution,
    #[fail(display = "IO Error")]
    IoError,
}

impl LexErrorKind {
    pub fn at(self, span: Span) -> LexError {
        LexError::new(self, span)
    }
}

#[derive(Debug, Clone, Fail)]
#[fail(display = "{} at {}", kind, span)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}
