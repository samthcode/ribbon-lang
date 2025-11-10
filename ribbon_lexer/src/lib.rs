use std::str::Chars;
use std::iter::Peekable;

pub mod tok;
pub use tok::Tok;

pub struct Lexer<'a> {
    cursor: Cursor<'a>
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer { cursor: Cursor::new(source) }
    }

    fn next_token(&mut self) -> Option<Tok> {
        if let Some(c) = self.cursor.peek() {
            match c {
                c if c.is_alphabetic() || *c == '_' => todo!(),
                _ => todo!()
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Tok;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    pos: u32
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        Cursor {
            chars: source.chars().peekable(),
            pos: 0
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;
        self.chars.next()
    }
}