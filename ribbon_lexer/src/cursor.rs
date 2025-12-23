use std::{iter::Peekable, str::Chars};

use crate::span::Span;

/// Peekable iterator over the characters of a source
#[derive(Clone)]
pub(crate) struct Cursor<'a> {
    pub source: &'a str,
    pub chars: Peekable<Chars<'a>>,
    pub pos: usize,
    pub curr: Option<char>,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut cursor = Cursor {
            source,
            chars: source.chars().peekable(),
            pos: 0,
            curr: None,
        };
        // Start on the first character
        cursor.next();
        // We want tokens to be zero-indexed
        cursor.pos = 0;
        cursor
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Returns a slice of the source code from the given position up to the current position
    pub fn source_from(&self, pos: usize) -> &'a str {
        &self.source[pos..=self.pos]
    }

    pub fn span_from(&self, pos: usize) -> Span {
        Span::new(pos, self.pos)
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;
        self.curr = self.chars.next();
        self.curr
    }
}
