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

macro_rules! next_or_eof {
    ($self:tt) => {
        $self
            .next_tok()
            .unwrap_or(Tok::new(TokKind::Eof, $self.eof_span()))
    };
}

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

    pub fn from_tok_stream(source: &'a str, tok_stream: TokStream<'a>) -> Self {
        Self {
            source,
            tok_stream: tok_stream.peekable(),
            program: Program::default(),
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(mut self) -> Program {
        loop {
            let maybe_expr = match self.peek_tok().cloned() {
                Some(t) if !t.is_eof() => match self.expr() {
                    Ok(expr) => {
                        match self.expect_one_of(&[TokKind::Op(OpKind::Semi), TokKind::Eof]) {
                            Ok(_) => Some(expr),
                            Err(e) => Some(Expr::new(
                                ExprKind::Invalid,
                                t.clone()
                                    .span
                                    .to(&self.report_and_recover_err(e, ParseCtx::Root)),
                            )),
                        }
                    }
                    Err(e) => Some(Expr::new(
                        ExprKind::Invalid,
                        t.clone()
                            .span
                            .to(&self.report_and_recover_err(e, ParseCtx::Root)),
                    )),
                },
                _ => break,
            };
            if let Some(expr) = maybe_expr {
                self.program.body.push(expr)
            }
        }
        self.program
    }

    fn expr(&mut self) -> Result<ast::Expr, Diagnostic> {
        self.expr_prec(binary_prec(&OpKind::Semi))
    }

    fn expr_prec(&mut self, min_prec: Prec) -> Result<ast::Expr, Diagnostic> {
        self.expr_prec_with_first(min_prec, None)
    }

    fn expr_prec_with_first(
        &mut self,
        min_prec: Prec,
        first: Option<Tok>,
    ) -> Result<ast::Expr, Diagnostic> {
        let lhs = if let Some(t) = first {
            t
        } else {
            // The Eof token has already been consumed
            // TODO: We should probably have a mechanism not to consume Eof unless it's needed
            // This would, however, require everything to peek before consuming
            // which isn't great for performance
            next_or_eof!(self)
        };
        assert!(!lhs.is_eof());
        let mut lhs = if let TokKind::Op(kind) = lhs.kind {
            match kind {
                // Block expression
                OpKind::LCurly => self.block_expr(lhs.span),
                // List expression
                OpKind::LSquare => {
                    let (exprs, span, _) = self.delimited_list(lhs.span, OpKind::RSquare)?;
                    Expr::new(ExprKind::List(exprs), span)
                }
                // Tuple/unit type/function parameter list
                OpKind::LParen => {
                    let (exprs, span, trailing_comma) =
                        self.delimited_list(lhs.span, OpKind::RParen)?;
                    if exprs.len() == 1 && !trailing_comma {
                        // We need the ParenthesisedExpression variant to ensure single-parameter functions are parsed correctly
                        Expr::new(
                            ExprKind::ParenthesisedExpression(Box::new(exprs[0].clone())),
                            span,
                        )
                    } else {
                        Expr::new(ExprKind::TupleOrParameterList(exprs), span)
                    }
                }
                // Unexpected (closing) delimiter
                op if op.is_delim() => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedDelimiter(op),
                        lhs.span,
                    ));
                }
                // Unary operator
                op @ (OpKind::Minus | OpKind::Bang | OpKind::Mul | OpKind::Amp) => {
                    if let Tok {
                        kind: TokKind::Eof,
                        span: _,
                    } = self
                        .peek_tok()
                        // Span here can't be eof_span() yet due to mutable self borrow in peek_tok()
                        .unwrap_or(&Tok::new(TokKind::Eof, Span::new(0, 0)))
                    {
                        return Err(Diagnostic::new_error(
                            ErrorKind::UnexpectedEofAfterBinaryOperator,
                            self.eof_span(),
                        ));
                    }
                    let rhs = self.expr_prec(unary_prec(&op))?;
                    let span = lhs.span.to(&rhs.span);
                    Expr::new(
                        ExprKind::UnaryOp {
                            rhs: Box::new(rhs),
                            kind: UnaryOp::new(
                                op.try_into().map_err(|_| {
                                    Diagnostic::new_error(
                                        ErrorKind::InvalidUnaryOperator(op),
                                        lhs.span,
                                    )
                                })?,
                                lhs.span,
                            ),
                        },
                        span,
                    )
                }
                k => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::InvalidUnaryOperator(k),
                        lhs.span,
                    ));
                }
            }
        } else {
            tok_to_expr(lhs)?
        };

        loop {
            let (op_kind, prec, op_span) = match self
                .peek_tok()
                .unwrap_or(&Tok::new(TokKind::Eof, lhs.span.hi.into()))
            {
                Tok {
                    kind: TokKind::Op(op_kind),
                    span,
                } => {
                    let prec = binary_prec(op_kind);
                    if prec <= min_prec {
                        break;
                    }
                    (*op_kind, prec, *span)
                }
                t if t.is_eof() => break,
                t => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedToken(t.kind.clone()),
                        t.span,
                    ));
                }
            };
            self.next_tok();
            // There are a few special cases that we need to parse differently
            match op_kind {
                // Function
                // TODO: Support `=>` functions e.g. `const fn = () => "Hello World".print;`
                OpKind::MinusGt => match lhs.kind {
                    ExprKind::TupleOrParameterList(exprs) => {
                        lhs = self.fn_decl(exprs, lhs.span, op_span)?;
                        continue;
                    }
                    ExprKind::ParenthesisedExpression(expr) => {
                        lhs = self.fn_decl(vec![*expr], lhs.span, op_span)?;
                        continue;
                    }
                    _ => {
                        return Err(Diagnostic::new_error(
                            ErrorKind::UnexpectedToken(TokKind::Op(op_kind)),
                            op_span,
                        ));
                    }
                },
                _ => (),
            }
            if let Tok {
                kind: TokKind::Eof,
                span: _,
            } = self
                .peek_tok()
                // Span here can't be eof_span() yet due to mutable self borrow in peek_tok()
                .unwrap_or(&Tok::new(TokKind::Eof, Span::new(0, 0)))
            {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedEofAfterBinaryOperator,
                    self.eof_span(),
                ));
            }
            let rhs = self.expr_prec(prec)?;
            let span = lhs.span.to(&rhs.span);
            lhs = Expr::new(
                ExprKind::BinOp {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: BinOp::new(
                        op_kind.try_into().map_err(|_| {
                            Diagnostic::new_error(
                                ErrorKind::InvalidBinaryOperator(op_kind),
                                op_span,
                            )
                        })?,
                        op_span,
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
    ) -> Result<Expr, Diagnostic> {
        let n_tok = self.peek_tok().unwrap();
        if n_tok.is_eof() {
            return Err(Diagnostic::new_error(
                ErrorKind::UnexpectedToken(n_tok.kind.clone()),
                n_tok.span,
            ));
        }
        let mut lcurly_span = n_tok.span;
        let ret_type = if let TokKind::Op(OpKind::LCurly) = n_tok.kind {
            // No return type
            self.next_tok();
            Expr::new(ExprKind::UnitType, arrow_span)
        } else {
            // Parse the return type up to the block
            let res = self.expr_prec(binary_prec(&OpKind::RCurly))?;
            lcurly_span = self
                .peek_tok()
                .unwrap_or(&Tok::new(
                    TokKind::Ident(Box::new("dummy".to_string())),
                    lcurly_span,
                ))
                .span;
            self.expect(TokKind::Op(OpKind::LCurly))?;
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
            param_span.to(&end_span),
        ))
    }

    fn block_expr(&mut self, begin_span: Span) -> Expr {
        let mut exprs = Vec::<Expr>::new();
        let mut end_span = begin_span;
        let mut unclosed = false;
        while let Some(t) = self.peek_tok() {
            if t.is_eof() {
                unclosed = true;
                break;
            }
            if t.kind.is(&TokKind::Op(OpKind::RCurly)) {
                self.next_tok();
                break;
            } else if t.kind.is(&TokKind::Op(OpKind::Semi)) {
                self.next_tok();
                continue;
            }
            let expr = self.expr_or_invalid(ParseCtx::Block);
            if expr.is(ExprKind::Invalid) {
                end_span = expr.span;
                exprs.push(expr);
                continue;
            }
            let eof = matches!(self.peek_tok().unwrap_or(&Tok::new(TokKind::Eof, Span::new(0,0))), t if t.is_eof());
            match self.expect_one_of(&[TokKind::Op(OpKind::Semi), TokKind::Op(OpKind::RCurly)]) {
                Ok(Tok { span, kind }) => {
                    end_span = span;
                    exprs.push(expr);
                    if matches!(self.peek_tok().unwrap_or(&Tok::new(TokKind::Eof, Span::new(0,0))), t if t.is_eof())
                    {
                        if !kind.is_op(&OpKind::RCurly) {
                            unclosed = true;
                        }
                        break;
                    }
                }
                Err(e) => {
                    end_span = e.span;
                    exprs.push(Expr::new(
                        ExprKind::Invalid,
                        expr.span
                            .to(&self.report_and_recover_err(e, ParseCtx::Block)),
                    ));
                    unclosed = eof
                }
            }
        }
        if unclosed {
            self.program.diagnostics.push(
                Diagnostic::new_error(
                    ErrorKind::UnclosedDelimitedExpression,
                    begin_span.to(&self.eof_span()),
                )
                .with_subdiagnostics(vec![Diagnostic::new_info(
                    InfoKind::DelimitedExpressionBeginsHere,
                    begin_span,
                )]),
            );
        }
        Expr::new(ExprKind::Block(exprs), begin_span.to(&end_span))
    }

    fn delimited_list(
        &mut self,
        begin_span: Span,
        closing_delim: OpKind,
    ) -> Result<(Vec<Expr>, Span, bool), Diagnostic> {
        let mut exprs: Vec<Expr> = vec![];
        // This is necessary for the differentiation of parenthesised expressions and tuples
        let mut trailing_comma = false;
        let end_span = loop {
            let t = next_or_eof!(self);
            if t.kind.is_eof() {
                // We don't want to simply return the error as we want to keep as much valid code as possible
                // We also don't need to perform error recovery as we are at Eof - there is nothing to recover to
                self.program.diagnostics.push(
                    Diagnostic::new_error(
                        ErrorKind::UnclosedDelimitedExpression,
                        begin_span.to(&t.span),
                    )
                    .with_subdiagnostics(vec![Diagnostic::new_info(
                        InfoKind::DelimitedExpressionBeginsHere,
                        begin_span,
                    )]),
                );
                break t.span;
            };

            if t.kind.is_op(&closing_delim) {
                break t.span;
            }

            let mut expr = self.expr_prec_with_first_or_invalid(
                binary_prec(&OpKind::Comma),
                Some(t),
                ParseCtx::DelimitedList(closing_delim),
            );
            if expr.is(ExprKind::Invalid) {
                exprs.push(expr);
                continue;
            }
            let Tok { kind, span } = match self
                .expect_one_of(&[TokKind::Op(OpKind::Comma), TokKind::Op(closing_delim)])
            {
                Ok(t) => {
                    exprs.push(expr);
                    t
                }
                Err(e) => {
                    expr.span = expr.span.to(&e.span);
                    exprs.push(expr);
                    self.report_and_recover_err(
                        e,
                        ParseCtx::DelimitedList(closing_delim.try_into().unwrap()),
                    );
                    continue;
                }
            };
            if kind.is_op(&closing_delim) {
                trailing_comma = false;
                break span;
            }
            trailing_comma = true
        };
        Ok((exprs, begin_span.to(&end_span), trailing_comma))
    }

    fn expr_or_invalid(&mut self, error_ctx: ParseCtx) -> Expr {
        self.expr_prec_or_invalid(binary_prec(&OpKind::Semi), error_ctx)
    }

    fn expr_prec_or_invalid(&mut self, min_prec: Prec, error_ctx: ParseCtx) -> Expr {
        self.expr_prec_with_first_or_invalid(min_prec, None, error_ctx)
    }

    /// Parses an expression or reports and recovers an error
    /// Returns the valid expression or an ExprKind::Invalid with span equal to the error Span
    fn expr_prec_with_first_or_invalid(
        &mut self,
        prec: Prec,
        first: Option<Tok>,
        error_ctx: ParseCtx,
    ) -> Expr {
        match self.expr_prec_with_first(prec, first) {
            Ok(expr) => expr,
            Err(e) => Expr::new(ExprKind::Invalid, self.report_and_recover_err(e, error_ctx)),
        }
    }

    /// Reports an error and attempts to recover based on the current context
    /// Returns the Span of the recovered section, not including a closing delimiter
    fn report_and_recover_err(&mut self, e: Diagnostic, ctx: ParseCtx) -> Span {
        let err_span = e.span;
        self.program.diagnostics.push(e);
        let ends = match ctx {
            ParseCtx::DelimitedList(delim) => &[TokKind::Op(OpKind::Comma), TokKind::Op(delim)],
            ParseCtx::Block => &[TokKind::Op(OpKind::Semi), TokKind::Op(OpKind::RCurly)],
            ParseCtx::Root => &[TokKind::Op(OpKind::Semi), TokKind::Eof],
        };
        let span = match self.skip_while(|t| !ends.contains(&t.kind)) {
            Some(s) => s,
            None => err_span,
        };
        // We've consumed the Eof token so we simply need to leave
        // The Eof should have already been handled
        if self.peek_tok().is_none() {
            return err_span;
        }
        match self.expect_one_of(ends) {
            Err(err) => {
                self.program.diagnostics.push(err);
                span
            }
            Ok(_) => span,
        }
    }

    /// Skip until the first token which matches the predicate is reached
    /// n.b. the first token matching the predicate is not consumed
    /// Returns an option which contains the Span of the skipped tokens, if any are skipped
    fn skip_while(&mut self, pred: impl Fn(&Tok) -> bool) -> Option<Span> {
        let mut m_start = None;
        while let Some(t) = self.peek_tok() {
            if !pred(t) {
                break;
            }
            if m_start.is_none() {
                m_start = Some(t.span)
            } else {
                m_start = m_start.map(|s| s.to(&t.span))
            }
            self.next_tok();
        }
        m_start
    }

    fn expect_one_of(&mut self, toks: &[TokKind]) -> Result<Tok, Diagnostic> {
        let t = self
            .next_tok()
            .unwrap_or(Tok::new(TokKind::Eof, self.eof_span()));
        if toks.iter().any(|tok| tok.is(&t.kind)) {
            Ok(t)
        } else {
            Err(Diagnostic::new_error(
                ErrorKind::ExpectedOneOfXFoundY(Vec::from(toks), t.kind),
                t.span,
            ))
        }
    }

    fn eof_span(&self) -> Span {
        if self.source.len() == 0 {
            Span::new(0, 0)
        } else {
            (self.source.len() as u32 - 1).into()
        }
    }

    fn expect(&mut self, tok: TokKind) -> Result<Tok, Diagnostic> {
        // This should never be called when Eof has been consumed
        let t = self.next_tok().unwrap();
        if t.kind.is(&tok) {
            Ok(t)
        } else {
            Err(Diagnostic::new_error(
                ErrorKind::ExpectedXFoundY(tok, t.kind),
                t.span,
            ))
        }
    }

    fn peek_tok(&mut self) -> Option<&Tok> {
        self.tok_stream.peek()
    }

    fn next_tok(&mut self) -> Option<Tok> {
        self.tok_stream.next()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseCtx {
    DelimitedList(OpKind),
    Block,
    Root,
}
