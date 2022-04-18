use self::ast::{AstNode, RootAstNode};
use crate::{
    error::{Error, ErrorKind},
    lexer::token::{Token, TokenKind},
    pos::{Pos, Span},
};
use std::vec;

pub mod ast;

pub struct Parser {
    pub tokens: Vec<Token>,
    ind: isize,
    errors: Vec<Error>,
    root: RootAstNode,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.into(),
            ind: -1,
            errors: Vec::new(),
            root: RootAstNode::new(vec![]),
        }
    }

    pub fn parse(&mut self) -> Result<RootAstNode, Vec<Error>> {
        while self.peek().is_some() {
            match self.parse_bp(0) {
                Ok(n) => self.root.push(n),
                Err(e) => self.error_and_recover(e),
            }
            self.skip_end_of_expr();
        }
        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }
        Ok(std::mem::take(&mut self.root))
    }

    #[allow(clippy::result_unit_err)]
    pub fn parse_bp(&mut self, rbp: u8) -> Result<AstNode, Error> {
        let mut lhs = match self.advance() {
            Some(t) => t.clone(),
            None => {
                let prev_loc = match self.prev() {
                    Some(t) => t.span.end,
                    None => Pos::with_values(1, 1),
                };
                return Err(Error::new(
                    Span::new(prev_loc, prev_loc),
                    ErrorKind::EofWhileParsing,
                ));
            }
        }
        .nud(self)?;

        while let Some(t) = self.peek() {
            // Here, we break if the original binding power exceeds the binding power of the next operator.
            if rbp >= t.bp().0 {
                break;
            }
            // Unwrap will never error here since we have already peeked to a valid token thank to `while let`
            lhs = self.advance().unwrap().clone().led(self, lhs);
        }

        Ok(lhs)
    }

    #[allow(clippy::result_unit_err)]
    pub fn expect(&mut self, kind: TokenKind) -> Result<Token, ()> {
        let t = self.advance().cloned();
        if t.is_none() || !kind.is_a(&t.as_ref().unwrap().kind) {
            let loc = match t.clone() {
                None => {
                    match self.prev() {
                        None => Span::from((1, 1, 1, 1)),
                        Some(t) => t.span
                    }
                }
                Some(s) => s.span,
            };
            let n = &t.map(|t| t.kind);
            self.error_and_recover(Error::new(
                loc,
                ErrorKind::ExpectedTokenFoundOther(kind, n.clone()),
            ));
            return Err(())
        }
        Ok(t.unwrap())
    }

    fn skip_end_of_expr(&mut self) {
        let t = match self.advance() {
            None => return,
            Some(t) => t.clone(),
        };
        match t.kind {
            TokenKind::Newline => {
                if let Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) = self.peek()
                {
                    self.advance();
                }
            }
            TokenKind::Semicolon => {
                if let Some(Token {
                    kind: TokenKind::Newline,
                    ..
                }) = self.peek()
                {
                    self.advance();
                }
            }
            k => self.errors.push(Error::new(
                t.span,
                ErrorKind::ExpectedEndOfExpressionFoundX(k),
            )),
        }
    }

    pub fn error_and_recover(&mut self, error: Error) {
        self.errors.push(error);
        self.recover_to_end_of_statement();
    }

    fn recover_to_end_of_statement(&mut self) {
        while let Some(t) = self.peek() {
            if t.kind == TokenKind::Newline || t.kind == TokenKind::Semicolon {
                return;
            }
            self.advance();
        }
    }

    #[inline(always)]
    pub fn advance(&mut self) -> Option<&Token> {
        self.ind += 1;
        self.tokens.get(usize::try_from(self.ind).unwrap()) // This will never be below 0 because of the above line, unless someone inits the Parser manually
    }

    #[inline(always)]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(usize::try_from(self.ind + 1).unwrap())
    }

    #[inline(always)]
    pub fn prev(&self) -> Option<&Token> {
        self.tokens.get(usize::try_from(self.ind - 1).unwrap_or(0))
    }
}
