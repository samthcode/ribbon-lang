use std::iter::Peekable;

use ribbon_ast::{self as ast, BinOp, Expr, ExprKind, UnaryOp, tok_to_expr};
use ribbon_error::{Diagnostic, ErrorKind, InfoKind};
use ribbon_lexer::{self as lexer, Lexer, OpKind, TokKind, TokStream, span::Span, tok::Tok};

use ast::Program;

mod prec;
use prec::{binary_prec, unary_prec};
#[cfg(test)]
mod test;

use crate::prec::Prec;

/// The parser for the Ribbon programming language.
///
/// This takes a source string (temporary) and first lexes it, and then turns
/// the resulting stream of tokens into an AST with a root Prorgam node.
pub struct Parser<'a> {
    source: &'a str,
    tok_stream: Peekable<TokStream<'a>>,
    program: Program<'a>,
    curr: Tok<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            tok_stream: Lexer::new(source).into_iter().peekable(),
            program: Default::default(),
            curr: Tok::dummy(),
        }
    }

    pub fn from_tok_stream(source: &'a str, tok_stream: TokStream<'a>) -> Self {
        Self {
            source,
            tok_stream: tok_stream.peekable(),
            program: Program::default(),
            curr: Tok::dummy(),
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(mut self) -> Program<'a> {
        loop {
            let t = self.next();
            if t.is_eof() {
                break;
            }
            let t_span = t.span;
            let expr = match self.expr_prec(binary_prec(&OpKind::Semi)) {
                Ok(expr) => match self.expect_one_of(&[TokKind::Op(OpKind::Semi), TokKind::Eof]) {
                    Ok(_) => expr,
                    Err(e) => Expr::new(
                        ExprKind::Invalid,
                        t_span.to(self.report_and_recover_err(e, ParseCtx::Root)),
                    ),
                },
                Err(e) => Expr::new(
                    ExprKind::Invalid,
                    t_span.to(self.report_and_recover_err(e, ParseCtx::Root)),
                ),
            };
            self.program.body.push(expr)
        }
        self.program
    }

    fn expr_prec(&mut self, min_prec: Prec) -> Result<ast::Expr, Diagnostic<'a>> {
        let lhs = &self.curr;
        let lhs_span = lhs.span;
        debug_assert!(!lhs.is_eof());
        let mut lhs = if let TokKind::Op(kind) = &lhs.kind {
            match kind {
                // Block expression
                OpKind::LCurly => self.block_expr(lhs_span),
                // List expression
                OpKind::LSquare => self.list_expr(lhs_span),
                // Tuple/unit type/function parameter list
                OpKind::LParen => self.tuple_like_expr(lhs_span),
                // Unary operator
                op @ (OpKind::Minus | OpKind::Bang | OpKind::Mul | OpKind::Amp) => {
                    self.unary_expr(*op, lhs_span)?
                }
                // Unexpected (closing) delimiter
                op if op.is_delim() => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedDelimiter(*op),
                        lhs_span,
                    ));
                }
                k => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedOperator(*k),
                        lhs_span,
                    ));
                }
            }
        } else {
            tok_to_expr(*lhs)?
        };

        loop {
            let n_tok = self.peek();
            let span = n_tok.span;
            if n_tok.is_eof() {
                break;
            }
            let TokKind::Op(op_kind) = n_tok.kind else {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(n_tok.clone()),
                    span,
                ));
            };
            let prec = binary_prec(&op_kind);
            if prec <= min_prec {
                break;
            }
            // There are a few special cases that we need to parse differently
            match op_kind {
                // Function
                // TODO: Support `=>` functions e.g. `const fn = () => "Hello World".print;`
                OpKind::MinusGt => match lhs.kind {
                    ExprKind::TupleOrParameterList(exprs) => {
                        self.next();
                        lhs = self.fn_decl(exprs, lhs.span, span)?;
                        continue;
                    }
                    ExprKind::ParenthesisedExpression(expr) => {
                        self.next();
                        lhs = self.fn_decl(vec![*expr], lhs.span, span)?;
                        continue;
                    }
                    _ => {
                        let n_tok = n_tok.clone();
                        self.next();
                        return Err(Diagnostic::new_error(
                            ErrorKind::UnexpectedToken(n_tok),
                            span,
                        ));
                    }
                },
                _ => {
                    self.next();
                }
            }
            if self.next().is_eof() {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedEofAfterBinaryOperator,
                    self.eof_span(),
                ));
            }
            let rhs = self.expr_prec(prec)?;
            let span = lhs.span.to(rhs.span);
            lhs = Expr::new(
                ExprKind::BinOp {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: BinOp::new(
                        op_kind.try_into().map_err(|_| {
                            Diagnostic::new_error(ErrorKind::InvalidBinaryOperator(op_kind), span)
                        })?,
                        span,
                    ),
                },
                span,
            )
        }
        Ok(lhs)
    }

    fn fn_decl(
        &mut self,
        parameters: Vec<Expr>,
        param_span: Span,
        arrow_span: Span,
    ) -> Result<Expr, Diagnostic<'a>> {
        let n_tok = self.next();
        if n_tok.is_eof() {
            return Err(Diagnostic::new_error(
                ErrorKind::UnexpectedToken(n_tok.clone()),
                self.eof_span(),
            ));
        }

        let mut lcurly_span = n_tok.span;
        let ret_type = if let TokKind::Op(OpKind::LCurly) = n_tok.kind {
            // No return type
            // TODO: This shouldn't be added in here, return type should be an option
            Expr::new(ExprKind::UnitType, arrow_span)
        } else {
            // Parse the return type up to the block
            let res = self.expr_prec(binary_prec(&OpKind::RCurly))?;
            lcurly_span = self.expect(TokKind::Op(OpKind::LCurly))?.span;
            res
        };

        // TODO: Support functions with fat arrow blocks
        let Expr {
            kind: ExprKind::Block(body),
            span: end_span,
        } = self.block_expr(lcurly_span)
        else {
            panic!()
        };
        Ok(Expr::new(
            ExprKind::FunctionDeclaration {
                parameters,
                generic_parameters: vec![],
                arrow_span,
                return_type: Box::new(ret_type),
                body,
            },
            param_span.to(end_span),
        ))
    }

    fn unary_expr(&mut self, op: OpKind, span: Span) -> Result<Expr, Diagnostic<'a>> {
        if self.next().is_eof() {
            return Err(Diagnostic::new_error(
                ErrorKind::UnexpectedEofAfterUnaryOperator,
                self.eof_span(),
            ));
        }
        let rhs = self.expr_prec(unary_prec(&op))?;
        let span = span.to(rhs.span);
        Ok(Expr::new(
            ExprKind::UnaryOp {
                rhs: Box::new(rhs),
                kind: UnaryOp::new(
                    // If this fails it is an internal error given we match a hardcoded set of unary ops
                    op.try_into().unwrap(),
                    span,
                ),
            },
            span,
        ))
    }

    fn tuple_like_expr(&mut self, span: Span) -> Expr {
        let (exprs, n_span, trailing_comma) = self.delimited_list(span, OpKind::RParen);
        if exprs.len() == 1 && !trailing_comma {
            // We need the ParenthesisedExpression variant to ensure single-parameter functions are parsed correctly
            Expr::new(
                ExprKind::ParenthesisedExpression(Box::new(exprs[0].clone())),
                n_span,
            )
        } else {
            Expr::new(ExprKind::TupleOrParameterList(exprs), n_span)
        }
    }

    fn list_expr(&mut self, span: Span) -> Expr {
        let (exprs, span, _) = self.delimited_list(span, OpKind::RSquare);
        Expr::new(ExprKind::List(exprs), span)
    }

    fn block_expr(&mut self, begin_span: Span) -> Expr {
        let mut exprs = Vec::<Expr>::new();
        let mut end_span = begin_span;
        let mut unclosed = true;
        loop {
            let t = self.next();
            if t.is_eof() {
                break;
            }
            end_span = t.span;
            if t.kind.is_op_kind(OpKind::RCurly) {
                unclosed = false;
                break;
            }
            if t.kind.is_op_kind(OpKind::Semi) {
                continue;
            }
            let expr = self.expr_or_invalid(ParseCtx::Block);
            if expr.is(ExprKind::Invalid) {
                end_span = expr.span;
                exprs.push(expr);
                continue;
            }
            let eof = self.peek().is_eof();
            match self.expect_one_of(&[TokKind::Op(OpKind::Semi), TokKind::Op(OpKind::RCurly)]) {
                Ok(Tok {
                    span,
                    kind,
                    source: _,
                }) => {
                    end_span = *span;
                    exprs.push(expr);
                    if kind.is_op_kind(OpKind::RCurly) {
                        unclosed = false;
                        break;
                    }
                }
                Err(e) => {
                    end_span = e.span;
                    exprs.push(Expr::new(
                        ExprKind::Invalid,
                        expr.span
                            .to(self.report_and_recover_err(e, ParseCtx::Block)),
                    ));
                    unclosed = eof
                }
            }
        }
        if unclosed {
            self.program.diagnostics.push(
                Diagnostic::new_error(
                    ErrorKind::UnclosedDelimitedExpression,
                    begin_span.to(end_span),
                )
                .with_subdiagnostics(vec![Diagnostic::new_info(
                    InfoKind::DelimitedExpressionBeginsHere,
                    begin_span,
                )]),
            );
        }
        Expr::new(ExprKind::Block(exprs), begin_span.to(end_span))
    }

    fn delimited_list(
        &mut self,
        begin_span: Span,
        closing_delim: OpKind,
    ) -> (Vec<Expr>, Span, bool) {
        let mut exprs: Vec<Expr> = vec![];
        // This is necessary for the differentiation of parenthesised expressions and tuples
        let mut trailing_comma = false;
        let end_span = loop {
            let t = self.next();
            let t_span = t.span;
            if t.kind.is_eof() {
                // We don't want to simply return the error as we want to keep as much valid code as possible
                // We also don't need to perform error recovery as we are at Eof - there is nothing to recover to
                self.program.diagnostics.push(
                    Diagnostic::new_error(
                        ErrorKind::UnclosedDelimitedExpression,
                        begin_span.to(t_span),
                    )
                    .with_subdiagnostics(vec![Diagnostic::new_info(
                        InfoKind::DelimitedExpressionBeginsHere,
                        begin_span,
                    )]),
                );
                break t_span;
            };

            if t.kind.is_op_kind(closing_delim) {
                break t.span;
            }

            let mut expr = self.expr_prec_or_invalid(
                binary_prec(&OpKind::Comma),
                ParseCtx::DelimitedList(closing_delim),
            );
            if expr.is(ExprKind::Invalid) {
                exprs.push(expr);
                continue;
            }
            let Tok {
                kind,
                span,
                source: _,
            } = match self.expect_one_of(&[TokKind::Op(OpKind::Comma), TokKind::Op(closing_delim)])
            {
                Ok(t) => {
                    exprs.push(expr);
                    t
                }
                Err(e) => {
                    expr.span = expr.span.to(e.span);
                    exprs.push(expr);
                    self.report_and_recover_err(
                        e,
                        ParseCtx::DelimitedList(closing_delim.try_into().unwrap()),
                    );
                    continue;
                }
            };
            if kind.is_op_kind(closing_delim) {
                trailing_comma = false;
                break *span;
            }
            trailing_comma = true
        };
        (exprs, begin_span.to(end_span), trailing_comma)
    }

    fn expr_or_invalid(&mut self, error_ctx: ParseCtx) -> Expr {
        self.expr_prec_or_invalid(binary_prec(&OpKind::Semi), error_ctx)
    }

    /// Parses an expression or reports and recovers an error
    /// Returns the valid expression or an ExprKind::Invalid with span equal to the error Span
    fn expr_prec_or_invalid(&mut self, prec: Prec, error_ctx: ParseCtx) -> Expr {
        match self.expr_prec(prec) {
            Ok(expr) => expr,
            Err(e) => Expr::new(ExprKind::Invalid, self.report_and_recover_err(e, error_ctx)),
        }
    }

    /// Reports an error and attempts to recover based on the current context
    /// Returns the Span of the recovered section, not including a closing delimiter
    fn report_and_recover_err(&mut self, e: Diagnostic<'a>, ctx: ParseCtx) -> Span {
        let err_span = e.span;
        self.program.diagnostics.push(e);
        let ends = match ctx {
            ParseCtx::DelimitedList(delim) => &[TokKind::Op(OpKind::Comma), TokKind::Op(delim)],
            ParseCtx::Block => &[TokKind::Op(OpKind::Semi), TokKind::Op(OpKind::RCurly)],
            ParseCtx::Root => &[TokKind::Op(OpKind::Semi), TokKind::Eof],
        };
        let span = match self.skip_while(|t| !ends.contains(&t.kind)) {
            Some(s) => err_span.to(s),
            None => err_span,
        };
        // We assume here that EOF will be handled further up the chain, so this avoids duplicate errors
        if self.peek().is_eof() {
            return span;
        }
        match self.expect_one_of(ends) {
            Err(err) => {
                self.program.diagnostics.push(err);
            }
            Ok(_) => (),
        }
        span
    }

    /// Skip until the first token which matches the predicate is reached
    /// n.b. the first token matching the predicate is not consumed
    /// Returns an option which contains the Span of the skipped tokens, if any are skipped
    fn skip_while(&mut self, pred: impl Fn(&Tok) -> bool) -> Option<Span> {
        let mut m_start = None;
        loop {
            let t = self.peek();
            if !pred(t) || t.is_eof() {
                break;
            }
            if m_start.is_none() {
                m_start = Some(t.span)
            } else {
                m_start = m_start.map(|s| s.to(t.span))
            }
            self.next();
        }
        m_start
    }

    fn expect_one_of(&mut self, toks: &[TokKind]) -> Result<&Tok<'a>, Diagnostic<'a>> {
        let t = self.next();
        if toks.iter().any(|kind| kind == &t.kind) {
            Ok(t)
        } else {
            Err(Diagnostic::new_error(
                ErrorKind::ExpectedOneOfXFoundY(Vec::from(toks), *t),
                t.span,
            ))
        }
    }

    fn eof_span(&self) -> Span {
        if self.source.len() == 0 {
            Span::new(0, 0)
        } else {
            (self.source.len() - 1).into()
        }
    }

    fn expect(&mut self, kind: TokKind) -> Result<&Tok<'a>, Diagnostic<'a>> {
        debug_assert!(!kind.is_eof());
        let t = self.next();
        if t.kind == kind {
            Ok(t)
        } else {
            Err(Diagnostic::new_error(
                ErrorKind::ExpectedXFoundY(kind, *t),
                t.span,
            ))
        }
    }

    /// Returns the next token in the token stream without consuming
    fn peek(&mut self) -> &Tok<'a> {
        if self.curr.is_eof() {
            return &self.curr;
        }
        // curr must be Eof for the iterator to be empty so this is safe
        self.tok_stream.peek().unwrap()
    }

    /// Consumes the next token in the iterator, stalling once the Eof token is reached
    fn next(&mut self) -> &Tok<'a> {
        if !self.curr.is_eof() {
            self.curr = self.tok_stream.next().unwrap();
        }
        &self.curr
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseCtx {
    DelimitedList(OpKind),
    Block,
    Root,
}
