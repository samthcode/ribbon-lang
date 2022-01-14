//! This module contains the structs used to define the positions of tokens and/or epxressions.
//!
//! # Basic Usage
//!
//! ```
//! use ribbon::pos;
//!
//! let mut pos1 = pos::Pos::new();
//! let pos2 = pos::Pos::new();
//! pos1.adv();
//!
//! let span = pos::Span::new(pos1, pos2);
//! ```

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new() -> Self {
        Self { line: 1, col: 0 }
    }
    pub fn with_values(line: usize, col: usize) -> Self {
        Self { line, col }
    }
    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }
    pub fn adv(&mut self) {
        self.col += 1
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// A Token's span from one position to another
/// For example, with a file containing "abcd", an Identifier token would be created
/// (Identifier("abcd")) with a span from line 1, col 1, to line 1, col 4

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}
