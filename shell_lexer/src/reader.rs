use crate::tokenenum::{LiteralMatcher, MatchResult};
use crate::{Pos, Span};
use anyhow::Error;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::io::{BufRead, BufReader, Read};

lazy_static! {
    static ref IO_NUMBER_RE: Regex =
        Regex::new(r"^[0-9]+[<>]").expect("failed to compile IO_NUMBER_RE");
    static ref ASSIGNMENT_WORD_RE: Regex =
        Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*=").expect("failed to compile ASSIGNMENT_WORD_RE");
}

pub struct CharReader<R: Read> {
    stream: BufReader<R>,
    line_buffer: String,
    line_idx: usize,
    position: Pos,
}

impl<R: Read> std::fmt::Debug for CharReader<R> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        fmt.debug_struct("CharReader")
            .field("line_buffer", &self.line_buffer)
            .field("line_idx", &self.line_idx)
            .field("position", &self.position)
            .finish()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PositionedChar {
    pub c: char,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum Next {
    Char(PositionedChar),
    Eof(Pos),
    Error(Error, Pos),
}

impl<R: Read> CharReader<R> {
    pub fn new(stream: R) -> Self {
        Self {
            stream: BufReader::new(stream),
            line_buffer: String::new(),
            line_idx: 0,
            position: Pos::new(0, 0),
        }
    }

    pub fn matches_literal<T: Copy>(
        &mut self,
        matcher: &LiteralMatcher<T>,
    ) -> anyhow::Result<MatchResult<T>> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(MatchResult::No),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => Ok(matcher.matches(&self.line_buffer[self.line_idx..])),
        }
    }

    pub fn next_literal<T: Copy>(
        &mut self,
        matcher: &LiteralMatcher<T>,
    ) -> anyhow::Result<Option<(T, Span)>> {
        match self.matches_literal(matcher)? {
            MatchResult::No => Ok(None),
            MatchResult::Match(value, len) => {
                self.line_idx += len;
                let start = self.position;
                let end = Pos::new(start.line, start.col + len - 1);
                self.position.col += len;
                Ok(Some((value, Span::new(start, end))))
            }
        }
    }

    pub fn matches_regex(&mut self, regex: &Regex) -> anyhow::Result<Option<(Captures, Pos)>> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(None),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => Ok(regex
                .captures(&self.line_buffer[self.line_idx..])
                .map(|c| (c, self.position))),
        }
    }

    /// Use this after calling matches_regex to fixup the matched length
    pub fn fixup_matched_length(&mut self, length: usize) {
        self.line_idx += length;
        self.position.col += length;
    }

    pub fn matches_io_number(&mut self) -> anyhow::Result<bool> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(false),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => Ok(IO_NUMBER_RE.is_match(&self.line_buffer[self.line_idx..])),
        }
    }

    pub fn next_io_number(&mut self) -> anyhow::Result<Option<(usize, Span)>> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(None),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => {
                if let Some(m) = IO_NUMBER_RE.find(&self.line_buffer[self.line_idx..]) {
                    let num_str = m.as_str();
                    let len = m.end() - 1;
                    let num = usize::from_str_radix(&num_str[..len], 10)
                        .expect("number to parse as number");
                    let start = self.position;
                    let end = Pos::new(start.line, start.col + len);
                    self.line_idx += len;
                    self.position.col += len;
                    Ok(Some((num, Span::new(start, end))))
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn matches_assignment_word(&mut self) -> anyhow::Result<bool> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(false),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => Ok(ASSIGNMENT_WORD_RE.is_match(&self.line_buffer[self.line_idx..])),
        }
    }

    pub fn next_assignment_word(&mut self) -> anyhow::Result<Option<(String, Span)>> {
        match self.check_and_fill_buffer() {
            Next::Eof(_) => Ok(None),
            Next::Error(err, pos) => return Err(err.context(pos).into()),
            _ => {
                if let Some(m) = ASSIGNMENT_WORD_RE.find(&self.line_buffer[self.line_idx..]) {
                    let num_str = m.as_str();
                    let len = m.end() - 1;
                    let name = num_str[..len].to_string();
                    let start = self.position;
                    let end = Pos::new(start.line, start.col + len + 1);
                    self.line_idx += len + 1;
                    self.position.col += len + 1;
                    Ok(Some((name, Span::new(start, end))))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn check_and_fill_buffer(&mut self) -> Next {
        if self.line_buffer.is_empty() || self.line_idx >= self.line_buffer.len() {
            let bump_line = !self.line_buffer.is_empty();
            self.line_buffer.clear();
            match self.stream.read_line(&mut self.line_buffer) {
                Ok(0) => return Next::Eof(self.position),
                Err(e) => return Next::Error(e.into(), self.position),
                _ => {
                    self.line_idx = 0;
                    self.position.col = 0;
                    if bump_line {
                        self.position.line += 1;
                    }
                }
            }
        }
        // Dummy result
        Next::Char(PositionedChar {
            c: ' ',
            pos: self.position,
        })
    }

    pub fn next_char(&mut self) -> Next {
        match self.check_and_fill_buffer() {
            fail @ Next::Eof(..) | fail @ Next::Error(..) => return fail,
            _ => {}
        }
        match (&self.line_buffer[self.line_idx..]).chars().next() {
            Some(c) => {
                let result = Next::Char(PositionedChar {
                    c,
                    pos: self.position,
                });
                self.line_idx += c.len_utf8();
                self.position.col += 1;
                result
            }
            None => {
                // No more to be read from this line; bump us over
                // the edge and recursse so that we trigger reading
                // the next line
                self.line_idx += 1;
                self.next_char()
            }
        }
    }

    pub fn unget(&mut self, c: PositionedChar) {
        let len = c.c.len_utf8();
        assert!(self.line_idx > 0);
        let (new_idx, overflow) = self.line_idx.overflowing_sub(len);
        assert!(
            !overflow,
            "overflowed while putting back a token: len={} line_idx={} c={:?} line_buffer={:?}",
            len, self.line_idx, c, self.line_buffer
        );
        self.line_idx = new_idx;
        self.position.col -= 1;
        assert_eq!(
            self.line_buffer[self.line_idx..].chars().next().unwrap(),
            c.c
        );
    }
}
