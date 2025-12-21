use std::iter::Peekable;

use ribbon_ast::{
    self as ast, BinOp, BinOpKind, Binding, Expr, ExprKind, FunctionDeclaration, FunctionTypeLike,
    Path, UnaryOp,
};
use ribbon_error::{Diagnostic, ErrorKind, InfoKind};
use ribbon_lexer::{self as lexer, Lexer, OpKind, TokKind, TokStream, span::Span, tok::Tok};

use ast::Program;

mod denotation;
mod narrow;
mod prec;

#[cfg(test)]
mod sexpr_test;
#[cfg(test)]
mod test;

use denotation::TryDenotation;
use narrow::TryNarrow;
use prec::{Prec, binary_prec, unary_prec};

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

    pub(crate) fn expr_prec(&mut self, min_prec: Prec) -> Result<Expr, Diagnostic<'a>> {
        // Get the left-hand side of the current expression
        let mut lhs = self.curr.try_null_denotation(self)?;

        loop {
            let n_tok = self.peek();
            if n_tok.is_eof() {
                break;
            }

            let op_span = n_tok.span;
            let TokKind::Op(op_kind) = n_tok.kind else {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(*n_tok),
                    op_span,
                ));
            };
            let prec = binary_prec(&op_kind);
            if prec <= min_prec {
                break;
            }

            // There are a few special cases that we need to parse differently

            // Binding with an explicit type
            if let ExprKind::BinOp(b) = &lhs.kind
                && b.kind == BinOpKind::MethodNoParen
                && op_kind == OpKind::Eq
            {
                // This avoids the borrow-checker/a very large nested if-statement
                let ExprKind::BinOp(b) = lhs.kind else {
                    unreachable!()
                };
                // Basically we have `(something):(something)` on the left and an `=` in the middle
                let BinOp {
                    lhs: pat, rhs: ty, ..
                } = *b;
                // TODO: Better pattern narrowing
                if !matches!(pat.kind, ExprKind::Ident(_)) {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedBindingOperator,
                        self.curr.span,
                    )
                    .with_subdiagnostics(vec![Diagnostic::new_info(
                        InfoKind::TryModifyingThisPattern,
                        lhs.span,
                    )]));
                }
                let ty = ty.try_narrow_to_type()?;
                // Consume the `=`
                self.next();
                // Move onto the next token
                self.next();
                let rhs = self.expr_prec(binary_prec(&OpKind::Eq))?;
                let span = pat.span.to(rhs.span);
                lhs = Expr::new(
                    ExprKind::Binding(Box::new(Binding::new(pat, Some(ty), rhs))),
                    span,
                );
                continue;
            }

            match op_kind {
                // Thin-arrow function type or declaration (`->`)
                OpKind::MinusGt => {
                    let parameters = match lhs.kind {
                        ExprKind::TupleOrParameterList(exprs) => exprs,
                        ExprKind::ParenthesisedExpression(expr) => vec![*expr],
                        _ => {
                            let n_tok = *n_tok;
                            self.next();
                            return Err(Diagnostic::new_error(
                                ErrorKind::UnexpectedToken(n_tok),
                                op_span,
                            ));
                        }
                    };
                    self.next();
                    lhs = self.fn_type_like_expr(parameters, lhs.span)?;
                    continue;
                }
                // Fat-arrow function declaration
                OpKind::EqGt => {
                    let parameters = match lhs.kind {
                        ExprKind::TupleOrParameterList(exprs) => exprs,
                        ExprKind::ParenthesisedExpression(expr) => vec![*expr],
                        _ => {
                            let n_tok = *n_tok;
                            self.next();
                            return Err(Diagnostic::new_error(
                                ErrorKind::UnexpectedToken(n_tok),
                                op_span,
                            ));
                        }
                    };
                    let parameters = self.validate_function_parameters(parameters);
                    // Consume `=>`
                    self.next();
                    // `expr_prec` works on the current token so we need to advance again
                    self.next();
                    let body = self.expr_prec(binary_prec(&OpKind::MinusGt))?;
                    let span = lhs.span.to(body.span);
                    lhs = Expr::new(
                        ExprKind::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                            parameters,
                            vec![],
                            None,
                            body,
                        ))),
                        span,
                    );
                    continue;
                }
                OpKind::Path => {
                    lhs = self.path(lhs)?;
                    continue;
                }
                _ => {
                    self.next();
                }
            }

            match self.next().kind {
                TokKind::Eof => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::UnexpectedEofAfterBinaryOperator,
                        self.eof_span(),
                    ));
                }
                // Another special case - a binding with `:=` (type is inferred)
                TokKind::Op(OpKind::Eq) if op_kind == OpKind::Colon => {
                    // TODO: Proper pattern narrowing
                    if !matches!(lhs.kind, ExprKind::Ident(_)) {
                        return Err(Diagnostic::new_error(
                            ErrorKind::UnexpectedBindingOperator,
                            self.curr.span,
                        )
                        .with_subdiagnostics(vec![Diagnostic::new_info(
                            InfoKind::TryModifyingThisPattern,
                            lhs.span,
                        )]));
                    }
                    self.next();
                    let rhs = self.expr_prec(binary_prec(&OpKind::Eq))?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::new(
                        ExprKind::Binding(Box::new(Binding::new(lhs, None, rhs))),
                        span,
                    );
                    continue;
                }
                _ => (),
            }
            let rhs = self.expr_prec(prec)?;
            let span = lhs.span.to(rhs.span);
            lhs = Expr::new(
                ExprKind::BinOp(Box::new(BinOp::try_from_op_kind(
                    op_kind, op_span, lhs, rhs,
                )?)),
                span,
            )
        }
        Ok(lhs)
    }

    /// Parses a path e.g. `a::b::c`
    /// n.b. This does not parse any following generic arguments since they are ambiguous at this stage
    pub(crate) fn path(&mut self, lhs: Expr) -> Result<Expr, Diagnostic<'a>> {
        if let ExprKind::Ident(_) = lhs.kind {
            // Consume `::`
            self.next();
            let mut span = lhs.span;
            let mut parts = vec![lhs];
            loop {
                // `a::b`
                //     ^
                let t = self
                    .expect(TokKind::Ident)?
                    .try_null_denotation(self)
                    .unwrap();
                span = span.to(t.span);
                parts.push(t);
                if !self.peek().is_op_kind(OpKind::Path) {
                    return Ok(Expr::new(ExprKind::Path(Path::new(parts)), span));
                }
                // Consume `::`
                self.next();
            }
        } else {
            Err(Diagnostic::new_error(
                ErrorKind::UnexpectedToken(self.curr),
                self.curr.span,
            ))
        }
    }

    /// Parses an expression such as `(a: i32, b: i32) -> i32`
    ///                                                ^^ `self.curr` required to be here
    pub(crate) fn fn_type_like_expr(
        &mut self,
        parameters: Vec<Expr>,
        param_span: Span,
    ) -> Result<Expr, Diagnostic<'a>> {
        debug_assert!(self.curr.is_op_kind(OpKind::MinusGt));
        self.next();

        // Expect a type after the arrow
        let return_type = self
            .expr_prec(binary_prec(&OpKind::EqGt))?
            .try_narrow_to_type();
        let return_type = self.report_and_invalidate_or_pass_through(return_type);

        let span = param_span.to(return_type.span);

        if self.peek().is_op_kind(OpKind::EqGt) {
            // Parse either an expression or a block
            // Consume `=>`
            self.next();
            // `expr_prec` works on the current token so we need to advance again
            self.next();
            let body = self.expr_prec(binary_prec(&OpKind::MinusGt))?;
            let span = param_span.to(body.span);
            let parameters = self.validate_function_parameters(parameters);
            Ok(Expr::new(
                ExprKind::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                    parameters,
                    vec![],
                    Some(return_type),
                    body,
                ))),
                span,
            ))
        } else if self.peek().is_op_kind(OpKind::LCurly) {
            // Consume `{`
            let t = self.next();
            let begin_span = t.span;
            let body = self.block_expr(begin_span);
            let span = param_span.to(body.span);
            let parameters = self.validate_function_parameters(parameters);

            Ok(Expr::new(
                ExprKind::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                    parameters,
                    vec![],
                    Some(return_type),
                    body,
                ))),
                span,
            ))
        } else {
            Ok(Expr::new(
                ExprKind::FunctionTypeLike(Box::new(FunctionTypeLike::new(
                    parameters,
                    vec![],
                    return_type,
                ))),
                span,
            ))
        }
    }

    pub(crate) fn validate_function_parameters(&mut self, parameters: Vec<Expr>) -> Vec<Expr> {
        parameters
            .into_iter()
            .map(|p| self.report_and_invalidate_or_pass_through(p.try_narrow_to_param()))
            .collect()
    }

    pub(crate) fn unary_expr(&mut self, op: OpKind, op_span: Span) -> Result<Expr, Diagnostic<'a>> {
        if self.next().is_eof() {
            return Err(Diagnostic::new_error(
                ErrorKind::UnexpectedEofAfterUnaryOperator,
                self.eof_span(),
            ));
        }
        let rhs = self.expr_prec(unary_prec(&op))?;
        let span = op_span.to(rhs.span);
        Ok(Expr::new(
            ExprKind::UnaryOp(Box::new(UnaryOp::new(op.try_into().unwrap(), op_span, rhs))),
            span,
        ))
    }

    pub(crate) fn tuple_like_expr(&mut self, span: Span) -> Expr {
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

    pub(crate) fn list_expr(&mut self, span: Span) -> Expr {
        let (exprs, span, _) = self.delimited_list(span, OpKind::RSquare);
        Expr::new(ExprKind::List(exprs), span)
    }

    pub(crate) fn block_expr(&mut self, begin_span: Span) -> Expr {
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

    pub(crate) fn delimited_list(
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
                    self.report_and_recover_err(e, ParseCtx::DelimitedList(closing_delim));
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

    pub(crate) fn expr_or_invalid(&mut self, error_ctx: ParseCtx) -> Expr {
        self.expr_prec_or_invalid(binary_prec(&OpKind::Semi), error_ctx)
    }

    /// Parses an expression or reports and recovers an error
    /// Returns the valid expression or an ExprKind::Invalid with span equal to the error Span
    pub(crate) fn expr_prec_or_invalid(&mut self, prec: Prec, error_ctx: ParseCtx) -> Expr {
        match self.expr_prec(prec) {
            Ok(expr) => expr,
            Err(e) => Expr::new(ExprKind::Invalid, self.report_and_recover_err(e, error_ctx)),
        }
    }

    /// Reports an error and attempts to recover based on the current context
    /// Returns the Span of the recovered section, not including a closing delimiter
    pub(crate) fn report_and_recover_err(&mut self, e: Diagnostic<'a>, ctx: ParseCtx) -> Span {
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
        if let Err(err) = self.expect_one_of(ends) {
            self.program.diagnostics.push(err);
        }
        span
    }

    pub(crate) fn report_and_invalidate_or_pass_through(
        &mut self,
        r: Result<Expr, Diagnostic<'a>>,
    ) -> Expr {
        match r {
            Ok(e) => e,
            Err(err) => {
                let span = err.span;
                self.program.diagnostics.push(err);
                Expr::new(ExprKind::Invalid, span)
            }
        }
    }

    /// Skip until the first token which matches the predicate is reached
    /// n.b. the first token matching the predicate is not consumed
    /// Returns an option which contains the Span of the skipped tokens, if any are skipped
    pub(crate) fn skip_while(&mut self, pred: impl Fn(&Tok) -> bool) -> Option<Span> {
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

    pub(crate) fn expect_one_of(&mut self, toks: &[TokKind]) -> Result<&Tok<'a>, Diagnostic<'a>> {
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

    pub(crate) fn eof_span(&self) -> Span {
        if self.source.is_empty() {
            Span::new(0, 0)
        } else {
            (self.source.len() - 1).into()
        }
    }

    pub(crate) fn expect(&mut self, kind: TokKind) -> Result<&Tok<'a>, Diagnostic<'a>> {
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
    pub(crate) fn peek(&mut self) -> &Tok<'a> {
        if self.curr.is_eof() {
            return &self.curr;
        }
        // curr must be Eof for the iterator to be empty so this is safe
        self.tok_stream.peek().unwrap()
    }

    /// Consumes the next token in the iterator, stalling once the Eof token is reached
    pub(crate) fn next(&mut self) -> &Tok<'a> {
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
