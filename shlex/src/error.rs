use std::io::{Error as IoError, ErrorKind as IoErrorKind};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPosition {
    pub line_number: usize,
    pub col_number: usize,
}

#[derive(Debug)]
pub struct LexError {
    pub position: TokenPosition,
    pub err: IoError,
}

impl LexError {
    pub fn from_io(err: IoError, position: TokenPosition) -> Self {
        Self { err, position }
    }

    pub fn with_message(message: &str, position: TokenPosition) -> Self {
        Self {
            err: IoError::new(IoErrorKind::Other, message),
            position,
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{} at line {} column {}",
            self.err, self.position.line_number, self.position.col_number
        )
    }
}
