use std::{error::Error, iter::Peekable};

use ribbon_ast::{self as ast, BinOp, Expr, ExprKind, UnaryOp, tok_to_expr};
use ribbon_error::{Diagnostic, ErrorKind, InfoKind};
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
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            tok_stream: Lexer::new(source).into_iter().peekable(),
            program: Default::default(),
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(mut self) -> Program {
        while let Some(_) = self.peek_tok() {
            match self.expr() {
                Ok(expr) => {
                    self.program.body.push(expr);
                    match self.expect_or_eof(TokKind::Op(OpKind::Semi)) {
                        Ok(()) => (),
                        Err(e) => self.program.diagnostics.push(e),
                    };
                }
                Err(e) => self.program.diagnostics.push(e),
            }
        }
        self.program
    }

    fn expr(&mut self) -> Result<ast::Expr, Box<dyn Error>> {
        self.expr_prec(Prec::new(PrecOrd::Semi, Fixity::None))
    }

    fn expr_prec(&mut self, min_prec: Prec) -> Result<ast::Expr, Box<dyn Error>> {
        let Some(lhs) = self.next_tok() else { panic!() };
        let mut lhs = if let TokKind::Op(kind) = lhs.kind {
            match kind {
                // List expression
                OpKind::LSquare => {
                    let (exprs, span, _) = self.list_expr(lhs.span, OpKind::RSquare)?;
                    Expr::new(ExprKind::List(exprs), span)
                }
                // Tuple/unit type/function parameter list
                OpKind::LParen => {
                    let (exprs, span, trailing_comma) = self.list_expr(lhs.span, OpKind::RParen)?;
                    if exprs.len() == 1 && !trailing_comma {
                        // It is a parenthesised expression
                        exprs[0].clone()
                    } else {
                        Expr::new(ExprKind::TupleOrParameterList(exprs), span)
                    }
                }
                // Unary operator
                op @ (OpKind::Minus | OpKind::Bang | OpKind::Mul | OpKind::Amp) => {
                    let rhs = self.expr_prec(unary_prec(&op))?;
                    let span = lhs.span.to(&rhs.span);
                    Expr::new(
                        ExprKind::UnaryOp {
                            rhs: Box::new(rhs),
                            kind: UnaryOp::new(op.try_into()?, lhs.span),
                        },
                        span,
                    )
                }
                k => panic!("unexpected/unimplemented unary op: {}", k.str()),
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
    ) -> Result<(Vec<Expr>, Span, bool), Box<dyn Error>> {
        let mut exprs: Vec<Expr> = vec![];
        let mut end_span = begin_span;
        // This is necessary for the differentiation of parenthesised expressions and tuples
        let mut trailing_comma = false;
        loop {
            let Some(t) = self.peek_tok() else {
                return Err(Box::new(
                    Diagnostic::new_error(
                        ErrorKind::UnclosedDelimitedExpression,
                        begin_span.to(&end_span),
                    )
                    .with_subdiagnostics(vec![Diagnostic::new_info(
                        InfoKind::DelimitedExpressionBeginsHere,
                        begin_span,
                    )]),
                ));
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
                    end_span = expr.span;
                    exprs.push(expr);
                    // We catch end of list in the next iteration so we don't want to consume it here
                    if !matches!(
                        self.peek_tok(),
                        Some(Tok {
                            kind: TokKind::Op(d),
                            span: _
                        }) if *d == delim
                    ) {
                        if let Some(Tok { kind, span }) = self.peek_tok() {
                            end_span = *span;
                            trailing_comma = matches!(kind, TokKind::Op(OpKind::Comma))
                        }
                        self.expect_one_of(&[TokKind::Op(OpKind::Comma), TokKind::Op(delim)])?;
                    }
                }
            }
        }
        // Consume end delimeter
        self.next_tok();
        Ok((exprs, begin_span.to(&end_span), trailing_comma))
    }

    fn expect_or_eof(&mut self, tok: TokKind) -> Result<(), Box<dyn Error>> {
        match self.next_tok() {
            Some(n_tok) => {
                if tok.is(&n_tok.kind) {
                    Ok(())
                } else {
                    Err(Box::new(Diagnostic::new_error(
                        ErrorKind::ExpectedXFoundY(tok, n_tok.kind),
                        n_tok.span,
                    )))
                }
            }
            None => Ok(()),
        }
    }

    fn expect_one_of(&mut self, toks: &[TokKind]) -> Result<(), Box<dyn Error>> {
        let n_tok = self.next_tok();
        if matches!(&n_tok, Some(t) if toks.iter().any(|tok| tok.is(&t.kind))) {
            return Ok(());
        }
        match n_tok {
            Some(t) => Err(Box::new(Diagnostic::new_error(
                ErrorKind::ExpectedOneOfXFoundY(Vec::from(toks), t.kind),
                t.span,
            ))),
            None => Err(Box::new(Diagnostic::new_error(
                ErrorKind::ExpectedOneOfXFoundEof(Vec::from(toks)),
                // TODO: this is wrong
                Span::new(0, 0),
            ))),
        }
    }

    fn expect(&mut self, tok: TokKind) -> Result<(), Box<dyn Error>> {
        match self.next_tok() {
            Some(t) if t.kind.is(&tok) => Ok(()),
            None => Err(Box::new(Diagnostic::new_error(
                ErrorKind::ExpectedXFoundEof(tok),
                // TODO: this is wrong
                Span::new(0, 0),
            ))),
            // TODO: Replace Debug call
            Some(t) => Err(Box::new(Diagnostic::new_error(
                ErrorKind::ExpectedXFoundY(tok, t.kind),
                t.span,
            ))),
        }
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
                for err in program.diagnostics.iter() {
                    eprintln!("{err}");
                }
                assert!(program.diagnostics.is_empty());
                assert_eq!(
                    program
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

    #[test]
    fn unary_expressions() {
        sexpr_test!("-10", "(- 10)");
        sexpr_test!("*ident", "(* ident)");
        sexpr_test!("*ident.func", "(* (. ident func))");
        sexpr_test!("-ident", "(- ident)");
        sexpr_test!("!ident + 10", "(+ (! ident) 10)");
        sexpr_test!("&ident", "(& ident)");
    }

    #[test]
    fn expression_or_tuple_or_parameter_list() {
        sexpr_test!("(1,2)", "(tuple-param-list 1 2)");
        sexpr_test!("(1,2,3,hello)", "(tuple-param-list 1 2 3 hello)");
        sexpr_test!("()", "(tuple-param-list)");
        sexpr_test!("(1)", "1");
        sexpr_test!("(hello+world)", "(+ hello world)");
    }
}
