use std::iter::Peekable;

use ribbon_ast::{
    BinOp, BinOpKind, Binding, Expr, ExprKind, FunctionDeclaration, FunctionType, Module, Path,
    UnaryOp,
};
use ribbon_error::{Diagnostic, ErrorKind, InfoKind};
use ribbon_lexer::{Lexer, OpKind, Tok, TokKind, TokStream, op, span::Span};

use crate::{
    denotation::TryDenotation,
    narrow::TryNarrow,
    prec::{Prec, binary_op_prec, binary_prec, unary_prec},
};

/// The parser for the Ribbon programming language.
///
/// This takes a source string or token stream and transforms it into a Ribbon module
pub struct Parser<'a> {
    source: &'a str,
    toks: Peekable<TokStream<'a>>,
    module: Module<'a>,
    curr: Tok<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            toks: Lexer::new(source).into_iter().peekable(),
            module: Module::default(),
            curr: Tok::dummy(),
        }
    }

    pub fn from_tok_stream(source: &'a str, tok_stream: TokStream<'a>) -> Self {
        Self {
            source,
            toks: tok_stream.peekable(),
            module: Module::default(),
            curr: Tok::dummy(),
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(mut self) -> Module<'a> {
        loop {
            let t = self.next();
            if t.is_eof() {
                break;
            }
            let t_span = t.span;
            let expr = self.expr_prec_or_invalid(binary_op_prec(op![;]), t_span, ParseCtx::Root);
            let expr = match self.expect_one_of(&[TokKind::Op(op![;]), TokKind::Eof]) {
                Ok(_) => expr,
                Err(e) => Expr::new(
                    ExprKind::Invalid,
                    t_span.to(self.report_and_recover_err(e, ParseCtx::Root)),
                ),
            };
            self.module.push_expr(expr);
        }
        self.module
    }

    pub(crate) fn expr_prec(&mut self, min_prec: Prec) -> Result<Expr, Diagnostic<'a>> {
        // Get the left-hand side of the current expression
        let lhs = self.curr.try_null_denotation(self);
        // If the left-hand side contains an error, we still attempt to parse the rest
        // This could potentially lead to some nonsensical errors further down the line since we (purposefully) don't recover
        let mut lhs = self.report_and_invalidate_or_inner(lhs);

        loop {
            let operator = self.peek();
            if operator.is_eof()
                || binary_prec(operator.kind).ok_or(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(*operator),
                    operator.span,
                ))? <= min_prec
            {
                break;
            }

            lhs = self.next().try_left_denotation(self, lhs)?;
        }
        Ok(lhs)
    }

    /// Parses a method call with optional parentheses `struct:method` or a binding `:=`
    ///
    /// `pattern: Type = rhs;` or `pattern := rhs` or `a:method`
    ///         ^ current token            ^            ^
    pub(crate) fn colon(&mut self, lhs: Expr) -> Result<Expr, Diagnostic<'a>> {
        debug_assert!(self.curr.is_op_kind(op![:]));
        let colon = self.curr;

        let ty = if self.next().is_op_kind(op![=]) {
            None
        } else {
            let rhs_or_type = self.expr_prec(binary_op_prec(op![:]));
            // It remains to be seen whether this could leave the parser in a state which raises more errors than needed
            // since the error is (purposefully) not recovered
            let rhs_or_type = self.report_and_invalidate_or_inner(rhs_or_type);
            let span = lhs.span.to(rhs_or_type.span);

            if self.peek().is_op_kind(op![=]) {
                self.next();
                Some(self.report_and_invalidate_or_inner(rhs_or_type.try_narrow_to_type()))
            } else {
                return Ok(Expr::new(
                    ExprKind::BinOp(Box::new(BinOp::new(
                        lhs,
                        rhs_or_type,
                        BinOpKind::MethodNoParen,
                        colon.span,
                    ))),
                    span,
                ));
            }
        };

        self.next();

        // Validate that the left-hand side is a valid pattern
        let lhs = self.report_and_invalidate_or_inner(lhs.try_narrow_to_pattern());

        let rhs = self.expr_prec(binary_op_prec(op![=]));
        let rhs = self.report_and_invalidate_or_inner(rhs);

        let span = lhs.span.to(rhs.span);

        Ok(Expr::new(
            ExprKind::Binding(Box::new(Binding::new(lhs, ty, rhs))),
            span,
        ))
    }

    /// Parses a function declaration or function type
    ///
    /// The current token is required to be one of two arrows:
    /// `() => "hello!"`
    ///     ^^ a fat arrow, like this
    /// `(a: i32, b: i32) -> i32` or `(a: i32: b:i32) -> i32 => a+b`
    ///                   ^^ a thin arrow, like these ^^
    /// A function type is defined as such:
    /// FN_TYPE <> (`[`(GENERIC_TY),*`]`)?`(`(TY),*`)` `->` TY
    /// And a function declaration is defined as:
    /// FN_DECL <> (`[`(GENERIC_TY),*`]`)?`(`(PARAM),*`)` (`->` TY)? (`=>` EXPR | BLOCK)
    /// n.b. EXPR could be a block expression
    pub(crate) fn function(&mut self, lhs: Expr) -> Result<Expr, Diagnostic<'a>> {
        let arrow = self.curr;
        debug_assert!(arrow.is_op_kind(op![->]) || arrow.is_op_kind(op![=>]));

        // There is always a parenthesised expression before a function arrow
        // `()` or `(PARAM|TY)` or `(PARAM_1, PARAM_2)` etc.
        let parameters = match lhs.kind {
            ExprKind::Tuple(exprs) => exprs,
            ExprKind::ParenthesisedExpression(expr) => vec![*expr],
            _ => {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(self.curr),
                    self.curr.span,
                ));
            }
        };

        let mut return_type = None;

        if arrow.is_op_kind(op![->]) {
            // We expect a type after the arrow
            self.next();
            let maybe_return_type = self
                .expr_prec(binary_op_prec(op![=>]))?
                .try_narrow_to_type();
            return_type = Some(self.report_and_invalidate_or_inner(maybe_return_type));

            let peeked_tok = self.peek();
            if !(peeked_tok.is_op_kind(op![=>]) || peeked_tok.is_op_kind(op!["{"])) {
                // We have a function type rather than a declaration
                let return_type = return_type.unwrap();
                let span = lhs.span.to(return_type.span);
                let parameters = parameters
                    .into_iter()
                    .map(|p| self.report_and_invalidate_or_inner(p.try_narrow_to_type()))
                    .collect();
                return Ok(Expr::new(
                    ExprKind::FunctionType(Box::new(FunctionType::new(
                        parameters,
                        vec![],
                        return_type,
                    ))),
                    span,
                ));
            }
            self.next();
        }

        let body = if self.curr.is_op_kind(op![=>]) {
            self.next();
            self.expr_prec(binary_op_prec(op![=>]))?
        } else {
            self.block_expr()
        };

        let parameters = parameters
            .into_iter()
            .map(|p| self.report_and_invalidate_or_inner(p.try_narrow_to_param()))
            .collect();

        let span = lhs.span.to(body.span);

        Ok(Expr::new(
            ExprKind::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                parameters,
                vec![],
                return_type,
                body,
            ))),
            span,
        ))
    }

    /// Parser a standard binary operator which has only a left-hand side, operator, and right-hand side
    pub(crate) fn binary_op(
        &mut self,
        op_kind: OpKind,
        op_span: Span,
        lhs: Expr,
    ) -> Result<Expr, Diagnostic<'a>> {
        self.next();
        let rhs = self.expr_prec(binary_op_prec(op_kind));
        // This may or may not be the best choice as it could leave the parser in an unexpected position later down the line
        let rhs = self.report_and_invalidate_or_inner(rhs);
        let span = lhs.span.to(rhs.span);
        Ok(Expr::new(
            ExprKind::BinOp(Box::new(BinOp::try_from_op_kind(
                op_kind, op_span, lhs, rhs,
            )?)),
            span,
        ))
    }

    /// Parses a path e.g. `a::b::c`
    /// n.b. This does not parse any following generic arguments since they are ambiguous at this stage
    pub(crate) fn path(&mut self, lhs: Expr) -> Result<Expr, Diagnostic<'a>> {
        if let ExprKind::Ident(_) = lhs.kind {
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
                if !self.peek().is_op_kind(op![::]) {
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
        let (exprs, n_span, trailing_comma) = self.delimited_list(span, op![")"]);
        if exprs.len() == 1 && !trailing_comma {
            // We need the ParenthesisedExpression variant to ensure single-parameter functions are parsed correctly
            Expr::new(
                ExprKind::ParenthesisedExpression(Box::new(exprs[0].clone())),
                n_span,
            )
        } else {
            Expr::new(ExprKind::Tuple(exprs), n_span)
        }
    }

    pub(crate) fn list_expr(&mut self, span: Span) -> Expr {
        let (exprs, span, _) = self.delimited_list(span, op!["]"]);
        Expr::new(ExprKind::List(exprs), span)
    }

    /// Parses a block expression
    ///
    /// A block expression is roughly defined as `{` (EXPR `;`)* EXPR? (`;`)? `}`
    /// i.e a list of expressions separated by semicolons, with an optional trailing semicolon
    /// FIXME: This trailing semicolon is significant as it determines if the block has a return value
    ///
    /// TODO: Add ability to parse struct literals
    ///       (although this may instead be handled by another function)
    pub(crate) fn block_expr(&mut self) -> Expr {
        debug_assert!(self.curr.is_op_kind(op!["{"]));
        let begin_span = self.curr.span;
        let mut exprs: Vec<Expr> = Vec::new();

        let mut should_expect = false;
        loop {
            let next = self.next();
            if next.kind.is_op_kind(op!["}"]) {
                return Expr::new(ExprKind::Block(exprs), begin_span.to(next.span));
            } else if next.kind.is_op_kind(op![;]) {
                // self.next();
                should_expect = false;
                continue;
            } else if should_expect {
                self.module.push_diagnostic(Diagnostic::new_error(
                    ErrorKind::ExpectedOneOfXFoundY(
                        vec![TokKind::Op(op![;]), TokKind::Op(op!["}"])],
                        self.curr,
                    ),
                    self.curr.span,
                ));
            }

            if self.curr.is_eof() {
                self.module.push_diagnostic(
                    Diagnostic::new_error(
                        ErrorKind::UnclosedDelimitedExpression,
                        begin_span.to(self.curr.span),
                    )
                    .with_subdiagnostics(vec![Diagnostic::new_info(
                        InfoKind::DelimitedExpressionBeginsHere,
                        begin_span,
                    )]),
                );
                return Expr::new(ExprKind::Block(exprs), begin_span.to(self.curr.span));
            }

            let expr =
                self.expr_prec_or_invalid(binary_op_prec(op![;]), begin_span, ParseCtx::Block);
            exprs.push(expr);
            should_expect = true;
        }
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
                self.module.diagnostics.push(
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
                break t_span;
            }

            let mut expr = self.expr_prec_or_invalid(
                binary_op_prec(op![,]),
                t_span,
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
            } = match self.expect_one_of(&[TokKind::Op(op![,]), TokKind::Op(closing_delim)]) {
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

    /// Parses an expression with a given precedence
    ///
    /// Returns a valid expression or an ExprKind::Invalid starting at the given span if there is an error
    /// n.b. This is mainly for use in the context of the root of a source or a block since it performs recovery
    pub(crate) fn expr_prec_or_invalid(
        &mut self,
        prec: Prec,
        begin_span: Span,
        error_ctx: ParseCtx,
    ) -> Expr {
        match self.expr_prec(prec) {
            Ok(expr) => expr,
            Err(e) => Expr::new(
                ExprKind::Invalid,
                begin_span.to(self.report_and_recover_err(e, error_ctx)),
            ),
        }
    }

    /// Reports an error and attempts to recover based on the current context
    /// Returns the Span of the recovered section, not including a closing delimiter
    pub(crate) fn report_and_recover_err(&mut self, e: Diagnostic<'a>, ctx: ParseCtx) -> Span {
        let err_span = e.span;
        self.module.diagnostics.push(e);
        let ends = match ctx {
            ParseCtx::DelimitedList(delim) => &[TokKind::Op(op![,]), TokKind::Op(delim)],
            ParseCtx::Block => &[TokKind::Op(op![;]), TokKind::Op(op!["}"])],
            ParseCtx::Root => &[TokKind::Op(op![;]), TokKind::Eof],
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
            self.module.diagnostics.push(err);
        }
        span
    }

    pub(crate) fn report_and_invalidate_or_inner(
        &mut self,
        r: Result<Expr, Diagnostic<'a>>,
    ) -> Expr {
        self.report_and_invalidate_spanned_or_inner(r, None)
    }

    pub(crate) fn report_and_invalidate_spanned_or_inner(
        &mut self,
        r: Result<Expr, Diagnostic<'a>>,
        begin_span: Option<Span>,
    ) -> Expr {
        match r {
            Ok(e) => e,
            Err(err) => {
                let span = if let Some(s) = begin_span {
                    s.to(err.span)
                } else {
                    err.span
                };
                self.module.diagnostics.push(err);
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
        self.toks.peek().unwrap()
    }

    /// Consumes the next token in the iterator, stalling once the Eof token is reached
    pub(crate) fn next(&mut self) -> &Tok<'a> {
        if !self.curr.is_eof() {
            self.curr = self.toks.next().unwrap();
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
