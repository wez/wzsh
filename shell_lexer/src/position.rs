use std::fmt::{Display, Error, Formatter};

/// A position within the input text
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl Display for Pos {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "line {} column {}", self.line, self.col)
    }
}

/// A token may span multiple positions; this struct
/// represents the span of such a thing.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new_pos(line: usize, col: usize) -> Self {
        let start = Pos::new(line, col);
        Self { start, end: start }
    }

    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    pub fn new_to(line: usize, col: usize, endcol: usize) -> Self {
        let start = Pos::new(line, col);
        let end = Pos::new(line, endcol);
        Self { start, end }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Span {
        Span::new_pos(pos.line, pos.col)
    }
}

impl Display for Span {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        if self.start == self.end {
            self.start.fmt(fmt)
        } else if self.start.line == self.end.line {
            write!(
                fmt,
                "line {} column {} thru {}",
                self.start.line, self.start.col, self.end.col
            )
        } else {
            write!(
                fmt,
                "line {} col {} thru line {} col {}",
                self.start.line, self.start.col, self.end.line, self.end.col
            )
        }
    }
}
