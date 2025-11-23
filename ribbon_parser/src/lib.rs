use std::{error::Error, iter::Peekable};

use ribbon_ast::{self as ast, BinOp, BinOpKind, Expr, ExprKind, tok_to_expr};
use ribbon_lexer::{self as lexer, Lexer, TokKind, TokStream, tok::Tok};

use ast::Program;

mod prec;
use prec::{Fixity, PrecOrd, binary_prec, unary_prec};

use crate::prec::Prec;

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
                Ok(expr) => {
                    self.program.body.push(expr);
                    self.next_tok();
                }
                Err(e) => self.errors.push(e),
            }
        }
        (self.program, self.errors)
    }

    fn expr(&mut self) -> Result<ast::Expr, Box<dyn Error>> {
        self.expr_prec(Prec::new(PrecOrd::Semi, Fixity::None))
    }

    fn expr_prec(&mut self, min_prec: Prec) -> Result<ast::Expr, Box<dyn Error>> {
        let Some(lhs) = self.next_tok() else { panic!() };
        // If the left hand side is an operator then we want to try to parse it as a unary operator.
        if let TokKind::Op(kind) = lhs.kind {
            todo!()
        }
        let mut lhs = tok_to_expr(lhs)?;
        loop {
            let tok = if let Some(t) = self.peek_tok() {
                t.clone()
            } else {
                break;
            };
            match tok {
                // If the next token is an operator we want to try to parse it as a binary operator
                Tok {
                    kind: TokKind::Op(op_kind),
                    span: op_span,
                } => {
                    let op_prec = binary_prec(&op_kind);
                    if op_prec < min_prec {
                        break;
                    }
                    self.next_tok();
                    let rhs = self.expr_prec(op_prec)?;
                    let span = lhs.span.to(&rhs.span);
                    lhs = Expr::new(
                        ExprKind::BinOp {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            kind: BinOp::new(op_kind.try_into()?, op_span),
                        },
                        span,
                    )
                }
                // If is not an operator then we raise an error
                tok => todo!(),
            }
        }
        Ok(lhs)
    }

    fn peek_tok(&mut self) -> Option<&Tok> {
        self.tok_stream.peek()
    }

    fn next_tok(&mut self) -> Option<Tok> {
        self.tok_stream.next()
    }
}
