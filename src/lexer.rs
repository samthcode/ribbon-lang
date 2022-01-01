pub mod token;

use crate::pos::Pos;
use core::iter::Peekable;
use std::str::Chars;

/// Turns a Ribbon file / &'a str into a Vec of tokens
pub struct Lexer<'a> {
    /// The iterator of characters which is contructed in the new() function
    chars: Peekable<Chars<'a>>,
    /// The current position of the Lexer, to be used in error handling
    pos: Pos,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            pos: Pos::new(),
        }
    }

    /// Turns the source file into a Vec of Token's
    pub fn lex(&mut self) -> Vec<token::Token> {
        todo!()
    }

    /// Advances the internal iterator and returns it's optional value
    /// This is also responsible for advancing the pos property appropriately
    /// (to be used in error handling)
    fn next(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                match c {
                    '\n' => self.pos.next_line(),
                    _ => self.pos.adv(),
                }
                Some(c)
            }
            None => None,
        }
    }

    /// Peeks to the next character to be lexed
    /// This does not advance the iterator
    fn peek(&mut self) -> Option<&char> {
        return self.chars.peek();
    }

    /// This yields characters while they match a predicate
    /// Warning: Advances the internal iterator
    fn take_while<F: Fn(char) -> bool>(&mut self, predicate: F) -> Vec<char> {
        // The resulting collected values
        let mut res: Vec<char> = Vec::new();

        // We use peek() here so that the first value not matching the predicate is kept for lexical analysis later on
        // Without peek(), this would consume the first non-matching character which could screw up the lexing
        while let Some(c) = self.peek() {
            if predicate(*c) {
                res.push(*c);
                self.next();
            } else {
                break;
            }
        }
        res
    }

    fn skip_excess_newlines(&mut self) {
        let _ = self.take_while(|c| c == '\n');
    }
}
