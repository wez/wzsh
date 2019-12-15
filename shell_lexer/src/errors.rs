use crate::position::Span;
use thiserror::*;

#[derive(Debug, Clone, Copy, Error)]
pub enum LexErrorKind {
    #[error("EOF while lexing backslash escape")]
    EofDuringBackslash,
    #[error("EOF while lexing comment")]
    EofDuringComment,
    #[error("EOF while lexing single quoted string")]
    EofDuringSingleQuotedString,
    #[error("EOF while lexing double quoted string")]
    EofDuringDoubleQuotedString,
    #[error("EOF while lexing parameter expansion")]
    EofDuringParameterExpansion,
    #[error("EOF while lexing assignment word")]
    EofDuringAssignmentWord,
    #[error("EOF while lexing command substitution")]
    EofDuringCommandSubstitution,
    #[error("IO Error")]
    IoError,
}

impl LexErrorKind {
    pub fn at(self, span: Span) -> LexError {
        LexError::new(self, span)
    }
}

#[derive(Debug, Clone, Error)]
#[error("{} at {}", kind, span)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}
