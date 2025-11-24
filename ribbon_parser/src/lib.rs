use std::{error::Error, iter::Peekable};

use ribbon_ast::{self as ast, BinOp, BinOpKind, Expr, ExprKind, tok_to_expr};
use ribbon_lexer::{self as lexer, Lexer, OpKind, TokKind, TokStream, span::Span, tok::Tok};

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
                    match self.expect_or_eof(TokKind::Op(OpKind::Semi)) {
                        Ok(()) => (),
                        Err(e) => self.errors.push(e),
                    };
                }
                Err(e) => self.errors.push(e),
            }
        }
        (self.program, self.errors)
    }

    fn expect_or_eof(&mut self, tok: TokKind) -> Result<(), Box<dyn Error>> {
        match self.next_tok() {
            Some(n_tok) => {
                if tok.is(&n_tok.kind) {
                    Ok(())
                } else {
                    Err(format!("Unexpected token {tok:?}").into())
                }
            }
            None => Ok(()),
        }
    }

    fn expect_one_of(&mut self, toks: &[TokKind]) -> Result<(), Box<dyn Error>> {
        let n_tok = self.next_tok();
        if matches!(n_tok, Some(t) if toks.iter().any(|tok| tok.is(&t.kind))) {
            return Ok(());
        }
        let formatted_toks = toks
            .iter()
            .map(|e| format!("{e}"))
            .reduce(|acc, e| format!("{acc}, {e}"))
            .unwrap();
        match self.next_tok() {
            // TODO: Replace Debug call
            Some(t) => {
                Err(format!("Expected one of {}, found {}.", formatted_toks, t.kind,).into())
            }
            None => Err(format!("Expected one of {}, found EOF.", formatted_toks).into()),
        }
    }

    fn expect(&mut self, tok: TokKind) -> Result<(), Box<dyn Error>> {
        match self.next_tok() {
            Some(t) if t.kind.is(&tok) => Ok(()),
            None => Err(format!("Expected {tok}, found EOF.").into()),
            // TODO: Replace Debug call
            Some(t) => Err(format!("Expected {tok}, found {t}.").into()),
        }
    }

    fn expr(&mut self) -> Result<ast::Expr, Box<dyn Error>> {
        self.expr_prec(Prec::new(PrecOrd::Semi, Fixity::None))
    }

    fn expr_prec(&mut self, min_prec: Prec) -> Result<ast::Expr, Box<dyn Error>> {
        let Some(lhs) = self.next_tok() else { panic!() };
        // If the left hand side is an operator then we want to try to parse it as a unary operator.
        let mut lhs = if let TokKind::Op(kind) = lhs.kind {
            match kind {
                OpKind::LSquare => {
                    let (exprs, span) = self.list_expr(lhs.span, OpKind::RSquare)?;
                    Expr::new(
                        ExprKind::List(exprs),
                        lhs.span, // TODO: wrong
                    )
                }
                _ => todo!(),
            }
        } else {
            tok_to_expr(lhs)?
        };

        loop {
            let (op_kind, prec, op_span) = match self.peek_tok() {
                Some(Tok {
                    kind: TokKind::Op(op_kind),
                    span,
                }) => {
                    let prec = binary_prec(op_kind);
                    if prec <= min_prec {
                        break;
                    }
                    (*op_kind, prec, *span)
                }
                Some(t) => {
                    return Err(format!("Unexpected token {t}. Expected binary operator.").into());
                }
                None => break,
            };
            self.next_tok();
            let rhs = self.expr_prec(prec)?;
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
        Ok(lhs)
    }

    fn list_expr(
        &mut self,
        begin_span: Span,
        delim: OpKind,
    ) -> Result<(Vec<Expr>, Span), Box<dyn Error>> {
        let mut exprs: Vec<Expr> = vec![];
        let end_span;
        loop {
            let Some(t) = self.peek_tok() else {
                return Err("EOF while parsing list.".into());
            };
            match t {
                // End of list
                Tok {
                    kind: TokKind::Op(d),
                    span,
                } if *d == delim => {
                    end_span = *span;
                    break;
                }
                // Get next element and push to exprs
                _ => {
                    let expr = self.expr_prec(binary_prec(&OpKind::Comma))?;
                    exprs.push(expr);
                    // We catch end of list in the next iteration so we don't want to consume it here
                    if !matches!(
                        self.peek_tok(),
                        Some(Tok {
                            kind: TokKind::Op(d),
                            span: _
                        }) if *d == delim
                    ) {
                        self.expect_one_of(&[
                            TokKind::Op(OpKind::Comma),
                            TokKind::Op(OpKind::RSquare),
                        ])?;
                    }
                }
            }
        }
        // Consume end delimeter
        self.next_tok();
        Ok((exprs, begin_span.to(&end_span)))
    }

    fn peek_tok(&mut self) -> Option<&Tok> {
        self.tok_stream.peek()
    }

    fn next_tok(&mut self) -> Option<Tok> {
        self.tok_stream.next()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    macro_rules! sexpr_test {
        ($src:literal, $($res:literal),+) => {
            {
                let program = Parser::new($src).parse();
                assert!(program.1.is_empty());
                assert_eq!(
                    program
                        .0
                        .body
                        .iter()
                        .map(|expr| expr.sexpr())
                        .collect::<Vec<String>>(),
                    vec![$($res, )+]
                );
            }
        };
    }
    #[test]
    fn binary_expressions() {
        sexpr_test!("1*2+3-4/5", "(- (+ (* 1 2) 3) (/ 4 5))")
    }

    #[test]
    fn lists() {
        sexpr_test!(
            "[1*10,2,\"Hello World\"+10]",
            "(list (* 1 10) 2 (+ \"Hello World\" 10))"
        )
    }
}
