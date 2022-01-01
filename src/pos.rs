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
//! let span = pos::Span::new(pos1, Some(pos2));
//! ```

pub struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    pub fn new() -> Self {
        Self { line: 1, col: 1 }
    }
    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }
    pub fn adv(&mut self) {
        self.col += 1
    }
}

/// A Token's span from one position to another
/// For example, with a file containing "abcd", an Identifier token would be created
/// (Identifier("abcd")) with a span from line 1, col 1, to line 1, col 4
pub struct Span {
    start: Pos,
    end: Option<Pos>,
}

impl Span {
    pub fn new(start: Pos, end: Option<Pos>) -> Self {
        Self { start, end }
    }
}
