//! This module contains all of the necessary utilities for Lexical Analysis of a Ribbon (.rbn) file.
//!
//! This module analyses Ribbon Source Code to create a Vector of Tokens while can then be passed to the Ribbon Parser.
//!
//! # Usage
//!
//! ```
//! use ribbon::lexer::Lexer;
//!
//! let mut lexer = Lexer::new("\"Hello World\"");
//! lexer.lex().unwrap();
//! ```

pub mod token;

use crate::{
    error::RibbonError,
    pos::{Pos, Span},
};
use core::iter::Peekable;
use std::str::Chars;

use self::token::{Token, TokenKind};

/// Turns a Ribbon file / &'a str into a Vec of tokens
pub struct Lexer<'a> {
    /// The iterator of characters which is contructed in the new() function
    chars: Peekable<Chars<'a>>,
    /// The current position of the Lexer, to be used in error handling
    pos: Pos,
    /// The errors which will be handled by the Ribbon Interpreter
    errors: Vec<RibbonError>,
    /// The tokens which will eventually be given to the user at the end of Lexer.lex()
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            pos: Pos::new(),
            errors: Vec::new(),
            tokens: Vec::new(),
        }
    }

    /// Turns the source file into a Vec of Token's
    pub fn lex(&mut self) -> Result<Vec<Token>, Vec<RibbonError>> {
        while let Some(c) = self.next() {
            match c {
                // Newline
                '\n' | '\r' => {
                    self.construct_newline(c);
                }

                // Any type of whitespace
                _ if c.is_whitespace() => (),

                // String
                '"' => self.construct_string(),

                // Character
                '\'' => self.construct_character(),

                // Single-character only operators
                ch @ ('.' | '{' | '}' | '(' | ')') => {
                    match self.peek() {
                        // If it's a dot and start of a float
                        Some(c) if c.is_numeric() && ch == '.' => self.construct_float(),
                        // TODO: Same with every other operator, add a check for a binding modifier
                        // TODO: This check will probably happen in a function called check_binding_modifier or something
                        // If it's just your plain old average operator
                        _ => self.tokens.push(Token::new(
                            ch.into(),
                            Span::new(self.pos.clone(), self.pos.clone()),
                        )),
                    }
                }

                // Colon or scope resolution operator
                c if c == ':' => {
                    match self.peek() {
                        Some(ch) if *ch == ':' => {
                            // It's a scope resolution operator

                            let start = self.pos.clone();
                            self.next();

                            self.tokens.push(Token::new(
                                TokenKind::ScopeResolutionOperator,
                                Span::new(start, self.pos.clone()),
                            ));
                        }
                        _ => {
                            // It's just a colon

                            self.tokens.push(Token::new(
                                TokenKind::Colon,
                                Span::new(self.pos.clone(), self.pos.clone()),
                            ))
                        }
                    }
                }

                // Numbers!
                c if c.is_numeric() => self.construct_number(c),

                // Identifier or keyword
                c if c.is_alphabetic() || c == '_' => self.construct_identifier_or_keyword(c),

                c => {
                    self.push_error_and_recover(RibbonError::new(
                        Span::new(self.pos.clone(), self.pos.clone()),
                        format!("Unexpected character: {}", c),
                    ));
                }
            }
        }

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(self.tokens.clone())
        }
    }

    /// Pushes an error onto the error stack then calls recover
    fn push_error_and_recover(&mut self, error: RibbonError) {
        self.errors.push(error);
        self.recover_error();
    }

    /// Allows the lexer to continue after an error has been found :)
    fn recover_error(&mut self) {
        // Just recover to the next line and collect some errors
        self.take_while(|ch| ch != '\n' || ch != '\r');
    }

    /// Constructs a character literal
    fn construct_character(&mut self) {
        let start = self.pos.clone();

        match self.next() {
            Some(c) => {
                if c == '\'' {
                    self.push_error_and_recover(RibbonError::new(
                        Span::new(start, self.pos.clone()),
                        String::from("Empty character literal."),
                    ));
                } else {
                    self.expect('\'');

                    self.tokens.push(Token::new(
                        TokenKind::Literal(token::LiteralKind::Char(c)),
                        Span::new(start, self.pos.clone()),
                    ));
                }
            }
            None => {
                self.push_error_and_recover(RibbonError::new(
                    Span::new(start, self.pos.clone()),
                    String::from("Unclosed charcter literal."),
                ));
            }
        }
    }

    /// Specifically constructs a float because you can do .1032032403 or whatever
    fn construct_float(&mut self) {
        let start = self.pos.clone();

        let decimal_part = self.take_while(|ch| ch.is_numeric());

        self.tokens.push(Token::new(
            TokenKind::Literal(token::LiteralKind::Float(
                (String::from("0.") + decimal_part.as_str())
                    .parse()
                    .unwrap_or_else(|_| {
                        panic!(
                            "(Ribbon Internal Error):{}: Couldn't parse float: '{}'",
                            self.pos.clone(),
                            decimal_part
                        )
                    }),
            )),
            Span::new(start, self.pos.clone()),
        ))
    }

    /// Constructs a number
    fn construct_number(&mut self, first: char) {
        let start = self.pos.clone();

        let mut result = String::new();

        result.push(first);

        result.push_str(self.take_while(|ch| ch.is_numeric()).as_str());

        if let Some('.') = self.peek() {
            self.next();

            result.push('.');

            result.push_str(self.take_while(|ch| ch.is_numeric()).as_str());

            self.tokens.push(Token::new(
                TokenKind::Literal(token::LiteralKind::Float(result.parse().unwrap())),
                Span::new(start, self.pos.clone()),
            ));
        } else {
            self.tokens.push(Token::new(
                TokenKind::Literal(token::LiteralKind::Integer(result.parse().unwrap())),
                Span::new(start, self.pos.clone()),
            ))
        }
    }

    /// Constructs a newline then skips excess newlines (this supports windows-style newlines (\r\n), although I don't yet know if it works...)
    fn construct_newline(&mut self, c: char) {
        // We need to tokenise one newline to check for EOS

        let pos = self.pos.clone();

        // The line number will be off by one since our next() method advances the line automatically, so we just subtract one to be accurate
        let correct_pos = Pos::with_values(pos.line - 1, pos.col);

        self.tokens.push(Token::new(
            TokenKind::Newline,
            Span::new(correct_pos, correct_pos),
        ));

        let carriage_return = if c == '\n' {
            false
        } else {
            self.next();
            true
        };

        // The rest of the newlines can be skipped
        self.skip_excess_newlines(carriage_return);
    }

    /// Constructs a string token
    ///
    /// Currently, we just take characters while said character is not a '"', however this is obviously flawed as it does not support escape literals
    fn construct_string(&mut self) {
        let start = self.pos.clone();
        let mut string = String::new();

        // TODO: This does not currenly support escape literals. Please make it do so!
        string.push_str(self.take_while(|char| char != '"').as_str());
        self.expect('"');

        self.tokens.push(Token::new(
            TokenKind::Literal(token::LiteralKind::String(string)),
            Span::new(start, self.pos.clone()),
        ));
    }

    /// Constructs an identifier token or searches the keyword hashmap for the identifier and constructs that if it's there
    fn construct_identifier_or_keyword(&mut self, first: char) {
        let start = self.pos.clone();
        let mut res = String::new();

        res.push(first);

        res.push_str(
            self.take_while(|ch| ch.is_alphanumeric() || ch == '_')
                .as_str(),
        );

        self.tokens.push(Token::new(
            if res == "true" {
                TokenKind::Literal(token::LiteralKind::Bool(true))
            } else if res == "false" {
                TokenKind::Literal(token::LiteralKind::Bool(false))
            } else if let Some(typ) = KEYWORD_MAP.get(&res) {
                TokenKind::Keyword(*typ)
            } else {
                TokenKind::Identifier(res)
            },
            Span::new(start, self.pos.clone()),
        ))
    }

    fn expect(&mut self, expected_char: char) {
        match self.next() {
            Some(c) if c == expected_char => (),
            Some(c) => {
                self.push_error_and_recover(RibbonError::new(
                    Span::new(self.pos.clone(), self.pos.clone()),
                    format!("Expected {}, found {}", expected_char, c),
                ));
            }
            None => {
                self.push_error_and_recover(RibbonError::new(
                    Span::new(self.pos.clone(), self.pos.clone()),
                    format!("Expected {}, found EOF", expected_char),
                ));
            }
        }
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
                    _ => panic!("(Ribbon Internal Error):{}: There wasn't a newline after the carriage return. WTF went wrong???", self.pos.clone())
                };
            }
        }
    }
}

static KEYWORD_MAP: phf::Map<&'static str, token::KeywordKind> = phf::phf_map! {
    "fn" => token::KeywordKind::Function,
    "if" => token::KeywordKind::If,
    "else" => token::KeywordKind::Else,
    "struct" => token::KeywordKind::Struct,
    "while" => token::KeywordKind::While,
    "ifp" => token::KeywordKind::Ifp,
    "whilep" => token::KeywordKind::Whilep,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn with_newline() {
        assert_eq!(
            Lexer::new("\n").lex().unwrap(),
            vec![Token::new(
                TokenKind::Newline,
                Span::new(Pos::with_values(1, 1), Pos::with_values(1,1))
            )]
        )
    }

    #[test]
    #[should_panic]
    fn charcter_too_long() {
        Lexer::new("'hello'").lex().unwrap();
    }

    #[test]
    fn character() {
        assert_eq!(
            Lexer::new("'a'").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Char('a')),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 3))
            )]
        )
    }

    #[test]
    #[should_panic]
    fn unclosed_character_literal() {
        Lexer::new("'").lex().unwrap();
    }

    #[test]
    fn booleans() {
        assert_eq!(
            Lexer::new("true").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Bool(true)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 4))
            )]
        );
        assert_eq!(
            Lexer::new("false").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Bool(false)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 5))
            )]
        );
    }

    #[test]
    fn hello_world() {
        assert_eq!(
            Lexer::new("\"Hello World!\".print").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::Literal(token::LiteralKind::String(String::from("Hello World!"))),
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 14))
                ),
                Token::new(TokenKind::Dot, Span::new(Pos::with_values(1, 15), Pos::with_values(1, 15))),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(1, 16), Pos::with_values(1, 20))
                )
            ]
        )
    }

    #[test]
    fn string_test() {
        assert_eq!(
            Lexer::new("\"Hello World\"").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::String(String::from("Hello World"))),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 13))
            )]
        )
    }

    #[test]
    fn fn_keyword() {
        assert_eq!(
            Lexer::new("fn").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::Function),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
            )]
        )
    }

    #[test]
    fn if_keyword() {
        assert_eq!(
            Lexer::new("if").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::If),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
            )]
        )
    }

    #[test]
    fn else_keyword() {
        assert_eq!(
            Lexer::new("else").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::Else),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 4))
            )]
        )
    }

    #[test]
    fn struct_keyword() {
        assert_eq!(
            Lexer::new("struct").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::Struct),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 6))
            )]
        )
    }

    #[test]
    fn while_keyword() {
        assert_eq!(
            Lexer::new("while").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::While),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 5))
            )]
        )
    }

    #[test]
    fn whilep_keyword() {
        assert_eq!(
            Lexer::new("whilep").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::Whilep),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 6))
            )]
        )
    }

    #[test]
    fn ifp_keyword() {
        assert_eq!(
            Lexer::new("ifp").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword(token::KeywordKind::Ifp),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 3))
            )]
        )
    }

    #[test]
    fn integer() {
        assert_eq!(
            Lexer::new("1234").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Integer(1234)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 4))
            )]
        )
    }

    #[test]
    fn long_integer() {
        assert_eq!(
            Lexer::new("1234453879834").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Integer(1234453879834)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 13))
            )]
        )
    }

    #[test]
    fn float_test() {
        assert_eq!(
            Lexer::new("1234453879834.342345345").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Float(1234453879834.342345345)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 23))
            )]
        )
    }

    #[test]
    fn float_starting_with_dot() {
        assert_eq!(
            Lexer::new(".342345345").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Float(0.342345345)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 10))
            )]
        )
    }
}
