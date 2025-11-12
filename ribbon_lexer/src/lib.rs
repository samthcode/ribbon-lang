use std::iter::Peekable;
use std::str::Chars;

#[macro_use]
pub mod tok;
pub use tok::{Tok, TokKind};

pub mod span;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    curr: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer {
            cursor: Cursor::new(source),
            curr: None,
        };
        // We want tok positions to be zero-indexed
        lexer.next_char();
        lexer.cursor.pos = 0;
        lexer
    }

    fn next_token(&mut self) -> Option<Tok> {
        let res = if let Some(c) = self.curr {
            match c {
                c if is_ident_head(&c) => Some(self.tok_ident()),
                _ => todo!(),
            }
        } else {
            None
        };
        self.next_char();
        res
    }

    /// Always use this instead of calling `self.cursor.next()` in order to maintain self.curr
    fn next_char(&mut self) {
        self.curr = self.cursor.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.cursor.peek()
    }

    fn tok_ident(&mut self) -> Tok {
        let mut res = self.curr.unwrap().to_string();
        let start = self.cursor.pos;
        while let Some(c) = self.peek_char() {
            if is_ident_tail(c) {
                res.push(*c);
            } else {
                // `?` can only appear at the end of a string so this must be the end
                if *c == '?' {
                    res.push(*c);
                    self.next_char();
                };
                break;
            }
            self.next_char();
        }
        tok!(Ident(res), start, self.cursor.pos)
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
    pos: u32,
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        Cursor {
            chars: source.chars().peekable(),
            pos: 0,
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

#[inline(always)]
pub fn is_ident_head(c: &char) -> bool {
    c.is_alphabetic() || *c == '_'
}

#[inline(always)]
pub fn is_ident_tail(c: &char) -> bool {
    c.is_alphanumeric() || *c == '_'
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test {
        ($source:literal, $($tok:expr),* $(,)?) => {
            assert_eq!(Lexer::new($source).into_iter().collect::<Vec<Tok>>(), vec![$(
                $tok,
            )*])
        }
    }

    #[test]
    fn ident() {
        test!("hello", tok!(Ident("hello".to_string()), 0, 4));
        test!("_hello", tok!(Ident("_hello".to_string()), 0, 5));
        test!("t1_var_?", tok!(Ident("t1_var_?".to_string()), 0, 7));
    }
}
