//! This module contains all of the necessary utilities for Lexical Analysis of a Ribbon (.rbn) file.
//!
//! This module analyses Ribbon Source Code to create a Vector of Tokens while can then be passed to the Ribbon Parser.
//!
//! # Usage
//!
//! ```
//! use ribbon::lexer::lexer;
//!
//! let lexer = Lexer::new("\"Hello World\"");
//! lexer.lex();
//! ```

pub mod token;

use crate::pos::{Pos, Span};
use core::iter::Peekable;
use std::{collections::HashMap, str::Chars};

/// Turns a Ribbon file / &'a str into a Vec of tokens
pub struct Lexer<'a> {
    /// The iterator of characters which is contructed in the new() function
    chars: Peekable<Chars<'a>>,
    /// The current position of the Lexer, to be used in error handling
    pos: Pos,
    /// The current file name, for error handling
    file: &'a str,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer
    pub fn new(source: &'a str, file: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            pos: Pos::new(),
            file,
        }
    }

    /// Turns the source file into a Vec of Token's
    pub fn lex(&mut self) -> Vec<token::Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.next() {
            match c {
                // Any type of whitespace
                _ if c.is_whitespace() => (),
                // String
                '"' => tokens.push(self.construct_string()),

                // The dot '.' operator
                '.' => {
                    // TODO: Same with every other operator, add a check for a binding modifier
                    // TODO: This check will probably happen in a function called check_binding_modifier or something
                    tokens.push(token::Token::new(
                        token::TokenKind::Dot,
                        Span::new(self.pos.clone(), None),
                    ))
                }

                // Identifier or keyword
                c if c.is_alphabetic() => {
                    tokens.push(self.construct_identifier_or_keyword(c));
                }

                // Newline
                '\n' | '\r' => {
                    tokens.push(self.construct_newline(c));
                }
                _ => {
                    eprintln!("{}:{}: Unexpected character '{}'", self.file, self.pos, c);
                    std::process::exit(1);
                }
            }
        }

        tokens
    }

    /// Constructs a newline then skips excess newlines (this supports windows-style newlines (\r\n), although I don't yet know if it works...)
    fn construct_newline(&mut self, c: char) -> token::Token {
        // We need to tokenise one newline to check for EOS
        let ret = token::Token::new(token::TokenKind::Newline, Span::new(self.pos.clone(), None));

        let carriage_return = if c == '\n' {
            false
        } else {
            self.next();
            true
        };

        // The rest of the newlines can be skipped
        self.skip_excess_newlines(carriage_return);

        ret
    }

    /// Constructs a string token
    ///
    /// Currently, we just take characters while said character is not a '"', however this is obviously flawed as it does not support escape literals
    fn construct_string(&mut self) -> token::Token {
        let start = self.pos.clone();
        let mut string = String::new();

        // TODO: This does not currenly support escape literals. Please make it do so!
        string.push_str(self.take_while(|char| char != '"').as_str());
        self.expect('"');

        token::Token::new(
            token::TokenKind::Literal(token::LiteralKind::String(string)),
            Span::new(start, Some(self.pos.clone())),
        )
    }

    /// Constructs an identifier token or searches the keyword hashmap for the identifier and constructs that if it's there
    fn construct_identifier_or_keyword(&mut self, first: char) -> token::Token {
        let start = self.pos.clone();
        let mut res = String::new();

        res.push(first);

        res.push_str(self.take_while(|ch| ch.is_alphanumeric()).as_str());

        let token_type = if let Some(typ) = keyword_map().get(&res) {
            token::TokenKind::Keyword(*typ)
        } else {
            token::TokenKind::Identifier(res)
        };

        token::Token::new(token_type, Span::new(start, Some(self.pos.clone())))
    }

    fn expect(&mut self, expected_char: char) {
        match self.next() {
            Some(c) if c == expected_char => return,
            Some(c) => {
                eprintln!("{}: Expected {}, found {}", self.pos, expected_char, c);
            }
            None => {
                eprintln!("{}: Expected {}, found EOF", self.pos, expected_char);
            }
        }
        std::process::exit(1);
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
    fn take_while<F: Fn(char) -> bool>(&mut self, predicate: F) -> String {
        // The resulting collected values
        let mut res = String::new();

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

    fn skip_excess_newlines(&mut self, is_carriage_return: bool) {
        if !is_carriage_return {
            self.take_while(|c| c == '\n');
            return;
        } else {
            while let Some('\r') = self.peek() {
                self.next();
                match self.next() {
                    Some('\n') => (),
                    _ => panic!("{}: There wasn't a newline after the carriage return. WTF went wrong??? (Internal error, but also what OS are you using that a newline doesn't follow a carriage return???)", self.pos.clone())
                };
            }
        }
    }
}

fn keyword_map() -> HashMap<String, token::KeywordKind> {
    use token::KeywordKind::*;
    // TODO: Add the other keywords in
    HashMap::from([("fn".to_string(), Function), ("if".to_string(), If)])
}
