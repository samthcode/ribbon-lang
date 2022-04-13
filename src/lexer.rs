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
    error::{Error, ErrorKind},
    pos::{Pos, Span},
};
use core::iter::Peekable;
use std::str::Chars;

use self::token::{LiteralKind, Token, TokenKind, KEYWORDS};

/// Turns a Ribbon file / &'a str into a Vec of tokens
pub struct Lexer<'a> {
    /// The iterator of characters which is contructed in the new() function
    chars: Peekable<Chars<'a>>,
    /// The current position of the Lexer, to be used in error handling
    pos: Pos,
    /// The errors which will be handled by the Ribbon Interpreter
    errors: Vec<Error>,
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
    pub fn lex(&mut self) -> Result<Vec<Token>, Vec<Error>> {
        while let Some(c) = self.next() {
            match c {
                // Newline
                '\n' | '\r' => {
                    self.construct_newline(c);
                }

                // Single-line Comments
                '/' if matches!(self.peek(), Some('/')) => {
                    // Consume the second /
                    self.next();

                    // Consume up to next line
                    self.take_while(|ch| ch != '\n' && ch != '\r');
                }

                // Multiline comments / Embedded comments
                // I would prefer these to be /* {comment} */ but that wouldn't work in certain situations
                // due to operator clumping (10 +/*Hello There*/ 20 making a '+', '/' and a '*' token)
                '#' => {
                    self.take_while(|ch| ch != '#');

                    self.expect('#');
                }

                // Any type of whitespace
                _ if c.is_whitespace() => (),

                // String
                '"' => self.construct_string(),

                // Character
                '\'' => self.construct_character(),

                // Operators
                c if token::OPERATOR_CHARACTERS.contains(&c) => {
                    let start = self.pos;
                    let mut clump = String::from(c);
                    clump.push_str(
                        self.take_while(|c| token::OPERATOR_CHARACTERS.contains(&c))
                            .as_str(),
                    );
                    self.make_operators(clump, start);
                }

                // Numbers!
                c if c.is_numeric() => self.construct_number(c),

                // Identifier or keyword
                c if c.is_alphabetic() || c == '_' => self.construct_identifier_or_keyword(c),

                c => {
                    self.raise_error_and_recover(Error::new(
                        Span::new(self.pos, self.pos),
                        ErrorKind::UnexpectedCharacter(c),
                    ));
                }
            }
        }

        // I can't be bothered to change all of the tests

        // self.tokens.push(Token::new(
        //     TokenKind::EOF,
        //     Span::new(self.pos.clone(), self.pos.clone()),
        // ));

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(self.tokens.clone())
        }
    }

    /// This takes a clump of characters (characters which csn be used within operatrs) and makes every token it needs to via recursion
    /// More specifically, this is acheived by finding the largest possible operator in the clump, appending it, then passing the remaining operators to this function again
    fn make_operators(&mut self, clump: String, start: Pos) {
        if clump.is_empty() {
            return;
        }
        for i in (0..=clump.len()).rev() {
            let mut maybe_op: String = clump.chars().take(i).collect();

            let mut binding_modified = false;
            if maybe_op.ends_with('$') {
                maybe_op = maybe_op.chars().take(i - 1).collect();
                binding_modified = true;
            }

            if let Ok(op_kind) = TokenKind::try_from(maybe_op) {
                let mut end = Pos::with_values(start.line, start.col + i - 1);
                self.tokens.push(Token::with_binding(
                    op_kind,
                    binding_modified,
                    Span::new(start, end),
                ));
                end.adv();
                self.make_operators(clump.chars().skip(i).collect(), end);
                return;
            }
        }

        self.raise_error_and_recover(Error::new(
            Span::new(
                start,
                Pos::with_values(start.line, start.col + clump.len() - 1),
            ),
            ErrorKind::InvalidOperator(clump),
        ))
    }

    /// Pushes an error onto the error stack then calls recover
    fn raise_error_and_recover(&mut self, error: Error) {
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
        let start = self.pos;

        match self.next() {
            Some(c) => {
                if c == '\'' {
                    self.raise_error_and_recover(Error::new(
                        Span::new(start, self.pos),
                        ErrorKind::InvalidLiteral(token::LiteralKind::Char(' ')),
                    ));
                } else {
                    if c == '\\' {
                        match self.next() {
                            None => {
                                self.errors.push(Error::new(
                                    Span::new(start, self.pos),
                                    ErrorKind::EofWhileLexingLiteral(LiteralKind::Char(' ')),
                                ));
                                return;
                            }
                            Some(c) => {
                                if let Ok(escape) = self.make_escape_character(c) {
                                    self.expect('\'');
                                    self.tokens.push(Token::new(
                                        TokenKind::Literal(token::LiteralKind::Char(escape)),
                                        Span::new(start, self.pos),
                                    ));
                                    return;
                                } else {
                                    self.raise_error_and_recover(Error::new(
                                        Span::new(start, self.pos),
                                        ErrorKind::InvalidEscapeCharacter(
                                            c,
                                            LiteralKind::Char(' '),
                                        ),
                                    ))
                                }
                            }
                        }
                    }
                    self.expect('\'');

                    self.tokens.push(Token::new(
                        TokenKind::Literal(token::LiteralKind::Char(c)),
                        Span::new(start, self.pos),
                    ));
                }
            }
            None => {
                self.raise_error_and_recover(Error::new(
                    Span::new(start, self.pos),
                    ErrorKind::EofWhileLexingLiteral(LiteralKind::Char(' ')),
                ));
            }
        }
    }

    /// Constructs a number
    fn construct_number(&mut self, first: char) {
        let start = self.pos;

        let mut result = String::new();

        result.push(first);

        result.push_str(self.take_while(|ch| ch.is_numeric()).as_str());

        if let Some('.') = self.peek() {
            self.next();

            result.push('.');

            result.push_str(self.take_while(|ch| ch.is_numeric()).as_str());

            self.tokens.push(Token::new(
                TokenKind::Literal(token::LiteralKind::Float(result.parse().unwrap_or_else(
                    |_| {
                        panic!(
                            "(Ribbon Internal Error):{}: Couldn't parse float: '{}'",
                            self.pos, result
                        )
                    },
                ))),
                Span::new(start, self.pos),
            ));
        } else {
            self.tokens.push(Token::new(
                TokenKind::Literal(token::LiteralKind::Integer(result.parse().unwrap_or_else(
                    |_| {
                        panic!(
                            "(Ribbon Internal Error):{}: Couldn't parse integer: '{}'",
                            self.pos, result
                        )
                    },
                ))),
                Span::new(start, self.pos),
            ))
        }
    }

    /// Constructs a newline then skips excess newlines (this supports windows-style newlines (\r\n), although I don't yet know if it works...)
    fn construct_newline(&mut self, c: char) {
        // We need to tokenise one newline to check for EOS

        let pos = self.pos;

        self.tokens
            .push(Token::new(TokenKind::Newline, Span::new(pos, pos)));

        let carriage_return = if c == '\n' {
            false
        } else {
            self.next();
            true
        };

        self.pos.next_line();

        // The rest of the newlines can be skipped
        self.skip_excess_newlines(carriage_return);
    }

    /// Constructs a string token
    ///
    /// Currently, we just take characters while said character is not a '"', however this is obviously flawed as it does not support escape literals
    fn construct_string(&mut self) {
        let start = self.pos;
        let mut string = String::new();

        let mut is_escape_sequence = false;
        while let Some(c) = self.next() {
            match c {
                '"' if !is_escape_sequence => {
                    self.tokens.push(Token::new(
                        TokenKind::Literal(token::LiteralKind::String(string)),
                        Span::new(start, self.pos),
                    ));
                    return;
                }
                '\n' => {
                    string.push('\n');
                    self.pos.next_line()
                }
                '\\' => {
                    if is_escape_sequence {
                        string.push('\\');
                        is_escape_sequence = false;
                    } else {
                        is_escape_sequence = true;
                    }
                }
                c if is_escape_sequence => {
                    if let Ok(escape) = self.make_escape_character(c) {
                        string.push(escape);
                        is_escape_sequence = false
                    } else {
                        self.raise_error_and_recover(Error::new(
                            Span::new(self.pos, self.pos),
                            ErrorKind::InvalidEscapeCharacter(
                                c,
                                LiteralKind::String("".to_string()),
                            ),
                        ));
                        return;
                    }
                }
                c => string.push(c),
            }
        }

        // No need to recover since it's EOF
        self.errors.push(Error::new(
            Span::new(self.pos, self.pos),
            ErrorKind::EofWhileLexingLiteral(LiteralKind::String(String::from(""))),
        ))
    }

    fn make_escape_character(&self, c: char) -> Result<char, ()> {
        match c {
            'n' => Ok('\n'),
            't' => Ok('\t'),
            'r' => Ok('\r'),
            '"' => Ok('"'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            _ => Err(()),
        }
    }

    /// Constructs an identifier token or searches the keyword hashmap for the identifier and constructs that if it's there
    fn construct_identifier_or_keyword(&mut self, first: char) {
        let start = self.pos;
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
            } else if KEYWORDS.contains(&&*res) {
                TokenKind::Keyword(res)
            } else {
                TokenKind::Identifier(res)
            },
            Span::new(start, self.pos),
        ))
    }

    fn expect(&mut self, expected_char: char) {
        match self.next() {
            Some(c) if c == expected_char => (),
            Some(c) if c == '\n' => {
                // Avoids 'error: Unexpeted character: '
                // '
                self.errors.push(Error::new(
                    Span::new(self.pos, self.pos),
                    ErrorKind::ExpectedXFoundY(expected_char, '\n'),
                ));
                self.pos.next_line();
            }
            Some(c) => {
                self.raise_error_and_recover(Error::new(
                    Span::new(self.pos, self.pos),
                    ErrorKind::ExpectedXFoundY(expected_char, c),
                ));
            }
            None => {
                self.raise_error_and_recover(Error::new(
                    Span::new(self.pos, self.pos),
                    ErrorKind::ExpectedXFoundEof(expected_char),
                ));
            }
        }
    }

    /// Advances the internal iterator and returns it's optional value
    /// This is also responsible for advancing the pos property appropriately
    /// (to be used in error handling)
    fn next(&mut self) -> Option<char> {
        self.pos.adv();
        self.chars.next()
    }

    /// Peeks to the next character to be lexed
    /// This does not advance the iterator
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// This yields characters while they match a predicate
    /// Warning: Advances the internal iterator
    fn take_while<F: Fn(char) -> bool>(&mut self, predicate: F) -> String {
        // The resulting collected values
        let mut res = String::new();

        // We use peek() here so that the first value not matching the predicate is kept for lexical analysis later on
        // Without peek(), this would consume the first non-matching character which could screw up the lexing
        while let Some(c) = self.peek() {
            let c = *c;
            if predicate(c) {
                res.push(c);
                self.next();
                if c == '\n' {
                    self.pos.next_line();
                }
            } else {
                break;
            }
        }
        res
    }

    fn skip_excess_newlines(&mut self, is_carriage_return: bool) {
        if !is_carriage_return {
            self.take_while(|c| c == '\n');
        } else {
            while let Some('\r') = self.peek() {
                self.next();
                match self.next() {
                    Some('\n') => self.pos.next_line(),
                    _ => panic!("(Ribbon Internal Error):{}: There wasn't a newline after the carriage return. WTF went wrong???", self.pos.clone())
                };
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{token::DelimKind, *};
    use pretty_assertions::assert_eq;
    use token::TokenKind::*;

    #[test]
    fn comments() {
        assert_eq!(Lexer::new("// Hello there").lex().unwrap(), vec![]);
        assert_eq!(Lexer::new("# Hello there #").lex().unwrap(), vec![]);
        assert_eq!(Lexer::new("#\nHello there\n#").lex().unwrap(), vec![]);
        assert_eq!(
            Lexer::new(
                "#
                Hello There
                This a multiline comment
                Lorem ipsum dolor sit amet
            #"
            )
            .lex()
            .unwrap(),
            vec![]
        );

        assert_eq!(
            Lexer::new("\"Hello World!\".print // prints \"Hello World!\"")
                .lex()
                .unwrap(),
            vec![
                Token::new(
                    TokenKind::Literal(token::LiteralKind::String(String::from("Hello World!"))),
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 14))
                ),
                Token::new(
                    TokenKind::Dot,
                    Span::new(Pos::with_values(1, 15), Pos::with_values(1, 15))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(1, 16), Pos::with_values(1, 20))
                )
            ]
        );
        assert_eq!(
            Lexer::new("\"Hello World!\".# Hello #print").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::Literal(token::LiteralKind::String(String::from("Hello World!"))),
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 14))
                ),
                Token::new(
                    TokenKind::Dot,
                    Span::new(Pos::with_values(1, 15), Pos::with_values(1, 15))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(1, 25), Pos::with_values(1, 29))
                )
            ]
        );
    }

    #[test]
    fn operators() {
        assert_eq!(
            Lexer::new(":: : + - / * += -= *= /=()").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::ScopeResolutionOperator,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
                ),
                Token::new(
                    TokenKind::Colon,
                    Span::new(Pos::with_values(1, 4), Pos::with_values(1, 4))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Add),
                    Span::new(Pos::with_values(1, 6), Pos::with_values(1, 6))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Sub),
                    Span::new(Pos::with_values(1, 8), Pos::with_values(1, 8))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Div),
                    Span::new(Pos::with_values(1, 10), Pos::with_values(1, 10))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Mul),
                    Span::new(Pos::with_values(1, 12), Pos::with_values(1, 12))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Add),
                    Span::new(Pos::with_values(1, 14), Pos::with_values(1, 15))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Sub),
                    Span::new(Pos::with_values(1, 17), Pos::with_values(1, 18))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Mul),
                    Span::new(Pos::with_values(1, 20), Pos::with_values(1, 21))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Div),
                    Span::new(Pos::with_values(1, 23), Pos::with_values(1, 24))
                ),
                Token::new(
                    TokenKind::try_from("(").unwrap(),
                    Span::from((1, 25, 1, 25))
                ),
                Token::new(
                    TokenKind::try_from(")").unwrap(),
                    Span::from((1, 26, 1, 26))
                )
            ]
        );
        assert_eq!(
            Lexer::new(":::+-/*+=-=*=/=**=()").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::ScopeResolutionOperator,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
                ),
                Token::new(
                    TokenKind::Colon,
                    Span::new(Pos::with_values(1, 3), Pos::with_values(1, 3))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Add),
                    Span::new(Pos::with_values(1, 4), Pos::with_values(1, 4))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Sub),
                    Span::new(Pos::with_values(1, 5), Pos::with_values(1, 5))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Div),
                    Span::new(Pos::with_values(1, 6), Pos::with_values(1, 6))
                ),
                Token::new(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Mul),
                    Span::new(Pos::with_values(1, 7), Pos::with_values(1, 7))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Add),
                    Span::new(Pos::with_values(1, 8), Pos::with_values(1, 9))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Sub),
                    Span::new(Pos::with_values(1, 10), Pos::with_values(1, 11))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Mul),
                    Span::new(Pos::with_values(1, 12), Pos::with_values(1, 13))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Div),
                    Span::new(Pos::with_values(1, 14), Pos::with_values(1, 15))
                ),
                Token::new(
                    TokenKind::ArithmeticOpEq(token::ArithmeticOpKind::Exp),
                    Span::new(Pos::with_values(1, 16), Pos::with_values(1, 18))
                ),
                Token::new(
                    TokenKind::try_from("(").unwrap(),
                    Span::from((1, 19, 1, 19))
                ),
                Token::new(
                    TokenKind::try_from(")").unwrap(),
                    Span::from((1, 20, 1, 20))
                ),
            ]
        )
    }

    #[test]
    fn binding_modifier() {
        assert_eq!(
            Lexer::new(":$ +$").lex().unwrap(),
            vec![
                Token::with_binding(
                    TokenKind::Colon,
                    true,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
                ),
                Token::with_binding(
                    TokenKind::ArithmeticOp(token::ArithmeticOpKind::Add),
                    true,
                    Span::new(Pos::with_values(1, 4), Pos::with_values(1, 5))
                )
            ]
        )
    }

    #[test]
    fn function() {
        assert_eq!(
            Lexer::new("main := () => {}").lex().unwrap(),
            vec![
                Token::new(Identifier("main".to_string()), Span::from((1, 1, 1, 4))),
                Token::new(Colon, Span::from((1, 6, 1, 6))),
                Token::new(Assignment, Span::from((1,7,1,7))),
                Token::new(OpenDelim(DelimKind::Parenthesis), Span::from((1, 9, 1, 9))),
                Token::new(
                    ClosingDelim(DelimKind::Parenthesis),
                    Span::from((1, 10, 1, 10))
                ),
                Token::new(FatArrow, Span::from((1, 12, 1, 13))),
                Token::new(OpenDelim(DelimKind::CurlyBracket), Span::from((1, 15, 1, 15))),
                Token::new(
                    ClosingDelim(DelimKind::CurlyBracket),
                    Span::from((1, 16, 1, 16))
                ),
            ]
        )
    }

    #[test]
    fn single_newline() {
        assert_eq!(
            Lexer::new("\n").lex().unwrap(),
            vec![Token::new(
                TokenKind::Newline,
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
            )]
        )
    }

    #[test]
    fn multiple_newlines() {
        assert_eq!(
            Lexer::new("\n\n\n\n").lex().unwrap(),
            vec![Token::new(
                TokenKind::Newline,
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
            )]
        )
    }

    #[test]
    fn windows_newlines() {
        assert_eq!(
            Lexer::new("\r\n\r\n\r\n").lex().unwrap(),
            vec![Token::new(
                TokenKind::Newline,
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
            )]
        )
    }

    #[test]
    fn newlines_surrounding_code() {
        assert_eq!(
            Lexer::new("\n\nprint\n").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(3, 1), Pos::with_values(3, 5))
                ),
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(3, 6), Pos::with_values(3, 6))
                )
            ]
        );
        assert_eq!(
            Lexer::new("\n\n\"Hello World!\".print\n\"String\"\n")
                .lex()
                .unwrap(),
            vec![
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
                ),
                Token::new(
                    TokenKind::Literal(token::LiteralKind::String(String::from("Hello World!"))),
                    Span::new(Pos::with_values(3, 1), Pos::with_values(3, 14))
                ),
                Token::new(
                    TokenKind::Dot,
                    Span::new(Pos::with_values(3, 15), Pos::with_values(3, 15))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(3, 16), Pos::with_values(3, 20))
                ),
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(3, 21), Pos::with_values(3, 21))
                ),
                Token::new(
                    TokenKind::Literal(token::LiteralKind::String(String::from("String"))),
                    Span::new(Pos::with_values(4, 1), Pos::with_values(4, 8))
                ),
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(4, 9), Pos::with_values(4, 9))
                )
            ]
        )
    }

    #[test]
    fn windows_newlines_surrounding_code() {
        assert_eq!(
            Lexer::new("\r\n\r\nprint\r\n").lex().unwrap(),
            vec![
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(1, 1), Pos::with_values(1, 1))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(3, 1), Pos::with_values(3, 5))
                ),
                Token::new(
                    TokenKind::Newline,
                    Span::new(Pos::with_values(3, 6), Pos::with_values(3, 6))
                )
            ]
        )
    }

    #[test]
    fn error_charcter_too_long() {
        if let Err(errs) = Lexer::new("'hello'").lex() {
            assert_eq!(errs.len(), 1);
            assert_eq!(
                *errs.get(0).unwrap(),
                Error::new(
                    Span::new(Pos::with_values(1, 3), Pos::with_values(1, 3)),
                    ErrorKind::ExpectedXFoundY('\'', 'e')
                )
            );
        } else {
            panic!()
        }
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
    fn unclosed_character_literal() {
        if let Err(errs) = Lexer::new("'").lex() {
            assert_eq!(errs.len(), 1);
            assert_eq!(
                errs.get(0).unwrap().span,
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
            );
        } else {
            panic!()
        }
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
                Token::new(
                    TokenKind::Dot,
                    Span::new(Pos::with_values(1, 15), Pos::with_values(1, 15))
                ),
                Token::new(
                    TokenKind::Identifier(String::from("print")),
                    Span::new(Pos::with_values(1, 16), Pos::with_values(1, 20))
                )
            ]
        )
    }

    #[test]
    fn string() {
        assert_eq!(
            Lexer::new("\"Hello World\"").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::String(String::from("Hello World"))),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 13))
            )]
        )
    }

    #[test]
    fn string_with_escape_characters() {
        assert_eq!(
            Lexer::new("\"Hello\\nWorld\\r\\t\"").lex().unwrap(),
            vec![Token::new(
                token::TokenKind::Literal(LiteralKind::String(String::from("Hello\nWorld\r\t"))),
                Span::from((1, 1, 1, 18))
            )]
        )
    }

    #[test]
    fn unclosed_string_literal() {
        if let Err(errs) = Lexer::new("\"Hello World").lex() {
            assert_eq!(errs.len(), 1);
            assert_eq!(
                errs.get(0).unwrap().span,
                Span::new(Pos::with_values(1, 13), Pos::with_values(1, 13))
            );
        } else {
            panic!()
        }
    }

    #[test]
    fn multiline_string() {
        assert_eq!(
            Lexer::new("\"Hello\nWorld\"").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::String(String::from("Hello\nWorld"))),
                Span::new(Pos::with_values(1, 1), Pos::with_values(2, 6))
            )]
        )
    }

    #[test]
    fn mut_keyword() {
        assert_eq!(
            Lexer::new("mut").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword("mut".to_string()),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 3))
            )]
        )
    }

    #[test]
    fn if_keyword() {
        assert_eq!(
            Lexer::new("if").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword("if".to_string()),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 2))
            )]
        )
    }

    #[test]
    fn else_keyword() {
        assert_eq!(
            Lexer::new("else").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword("else".to_string()),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 4))
            )]
        )
    }

    #[test]
    fn while_keyword() {
        assert_eq!(
            Lexer::new("while").lex().unwrap(),
            vec![Token::new(
                TokenKind::Keyword("while".to_string()),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 5))
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
    fn float() {
        assert_eq!(
            Lexer::new("1234453879834.342").lex().unwrap(),
            vec![Token::new(
                TokenKind::Literal(token::LiteralKind::Float(1234453879834.342)),
                Span::new(Pos::with_values(1, 1), Pos::with_values(1, 17))
            )]
        )
    }
}
