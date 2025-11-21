use std::{error::Error, iter::Peekable};

use ribbon_ast::{self as ast, tok_to_expr};
use ribbon_lexer::{self as lexer, Lexer, TokKind, TokStream, tok::Tok};

use ast::Program;

mod prec;
use prec::{Fixity, Prec, binary_prec, unary_prec};

/// The parser for the Ribbon programming language.
///
/// This takes a source string (temporary) and first lexes it, and then turns
/// the resulting stream of tokens into an AST with a root Prorgam node.
pub struct Parser<'a> {
    source: &'a str,
    tok_stream: Peekable<TokStream<'a>>,
    program: Program,
    errors: Vec<Box<dyn Error>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            tok_stream: Lexer::new(source).into_iter().peekable(),
            program: Default::default(),
            errors: vec![],
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(mut self) -> (Program, Vec<Box<dyn Error>>) {
        while let Some(_) = self.peek_tok() {
            match self.expr() {
                Ok(expr) => self.program.body.push(expr),
                Err(e) => self.errors.push(e),
            }
        }
        (self.program, self.errors)
    }

    fn expr(&mut self) -> Result<ast::Expr, Box<dyn Error>> {
        self.expr_prec((Prec::Semi, Fixity::None))
    }

    fn expr_prec(&mut self, (prec, fixity): (Prec, Fixity)) -> Result<ast::Expr, Box<dyn Error>> {
        let Some(lhs) = self.next_tok() else { panic!() };
        if let TokKind::Op(kind) = lhs.kind {
            // Unary operator
            todo!()
        }
        match self.next_tok() {
            Some(Tok {
                kind: TokKind::Op(op_kind),
                span,
            }) => {
                let rhs = self.expr_prec(binary_prec(op_kind))?;
            }
            Some(tok) => todo!(), // Error
            None => return Ok(tok_to_expr(lhs).unwrap()),
        }
        todo!()
    }

    fn peek_tok(&mut self) -> Option<&Tok> {
        self.tok_stream.peek()
    }

    fn next_tok(&mut self) -> Option<Tok> {
        self.tok_stream.next()
    }
}
