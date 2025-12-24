use std::fmt::Display;

use ribbon_lexer::{OpKind, Tok, TokKind, span::Span, tok::InvalidStrKind};

use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Diagnostic<'a> {
    pub kind: DiagnosticKind<'a>,
    pub span: Span,
    subdiagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> Diagnostic<'a> {
    pub fn new(kind: DiagnosticKind<'a>, span: Span) -> Self {
        Self {
            kind,
            span,
            subdiagnostics: vec![],
        }
    }

    pub fn new_error(kind: ErrorKind<'a>, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::<'a>::Error(kind),
            span,
            subdiagnostics: vec![],
        }
    }

    pub fn new_warning(kind: WarningKind, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Warning(kind),
            span,
            subdiagnostics: vec![],
        }
    }

    pub fn new_info(kind: InfoKind, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Info(kind),
            span,
            subdiagnostics: vec![],
        }
    }

    pub fn with_subdiagnostics(self, subdiagnostics: Vec<Diagnostic<'a>>) -> Self {
        Self {
            subdiagnostics,
            ..self
        }
    }
}

impl<'a> Display for Diagnostic<'a> {
    // TODO: This is temporary
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.span, self.kind)?;
        for sub in &self.subdiagnostics {
            write!(f, "\n{}", sub)?
        }
        write!(f, "")
    }
}

#[derive(Debug, Serialize)]
pub enum DiagnosticKind<'a> {
    Error(ErrorKind<'a>),
    Warning(WarningKind),
    Info(InfoKind),
}

impl<'a> Display for DiagnosticKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                DiagnosticKind::Error(error_kind) => format!("{}", error_kind),
                DiagnosticKind::Warning(warning_kind) => format!("{}", warning_kind),
                DiagnosticKind::Info(info_kind) => format!("{}", info_kind),
            }
        )
    }
}

#[derive(Debug, Serialize)]
pub enum ErrorKind<'a> {
    ExpectedXFoundY(TokKind, Tok<'a>),
    ExpectedOneOfXFoundY(Vec<TokKind>, Tok<'a>),
    UnclosedDelimitedExpression,
    UnexpectedToken(Tok<'a>),
    UnexpectedOperator(OpKind),
    UnexpectedBindingOperator,
    InvalidBinaryOperator(OpKind),
    UnexpectedEofAfterBinaryOperator,
    InvalidStringLiteral(InvalidStrKind),
    UnexpectedDelimiter(OpKind),
    UnexpectedEofAfterUnaryOperator,
    ExpectedTypeFoundOperator(OpKind),
    // Unfortunately now we're getting to errors which involve `Expr`s.
    // An `Expr` can't be held in this enum as it would require a circular dependency.
    // TODO: Figure out the best solution.
    ExpectedFunctionParameterFoundX(&'static str),
    ExpectedTypeFoundX(&'static str),
    ExpectedPatternFoundX(&'static str),
}

impl<'a> Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // FIXME: Replace use of Debug
                ErrorKind::ExpectedXFoundY(expected, found) =>
                    format!("expected {}, found {}", expected, found),
                ErrorKind::ExpectedOneOfXFoundY(expected, found) => {
                    format!(
                        "expected one of {}, found {}",
                        expected
                            .iter()
                            .map(|e| format!("{e}"))
                            .reduce(|acc, e| format!("{acc}, {e}"))
                            .unwrap(),
                        found
                    )
                }
                ErrorKind::UnclosedDelimitedExpression =>
                    "unclosed delimited expression".to_string(),
                ErrorKind::UnexpectedToken(tok) => format!("unexpected token `{}`", tok),
                ErrorKind::UnexpectedOperator(op) => format!("unexpected operator `{}`", op.str()),
                ErrorKind::UnexpectedBindingOperator => "unexpected binding operator".to_string(),
                ErrorKind::InvalidBinaryOperator(op) =>
                    format!("invalid binary operator `{}`", op.str()),
                ErrorKind::InvalidStringLiteral(k) => match k {
                    InvalidStrKind::Unclosed => "unclosed string literal".to_string(),
                    InvalidStrKind::UnterminatedEscape =>
                        "unterminated escape sequence in string literal".to_string(),
                },
                ErrorKind::UnexpectedDelimiter(delim) =>
                    format!("unexpected delimiter `{}`", delim.str()),
                ErrorKind::UnexpectedEofAfterBinaryOperator =>
                    "unexpected end of file after binary operator".to_string(),
                ErrorKind::UnexpectedEofAfterUnaryOperator =>
                    "unexpected end of file after unary operator".to_string(),
                ErrorKind::ExpectedTypeFoundOperator(op) =>
                    format!("expected type, found operator `{}`", op.str()),
                ErrorKind::ExpectedFunctionParameterFoundX(s) =>
                    format!("expected function parameter, found {s}"),
                ErrorKind::ExpectedTypeFoundX(s) => format!("expected type, found {s}"),
                ErrorKind::ExpectedPatternFoundX(s) => format!("expected pattern, found {s}"),
            }
        )
    }
}

#[derive(Debug, Serialize)]
pub enum WarningKind {}

impl Display for WarningKind {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Serialize)]
pub enum InfoKind {
    DelimitedExpressionBeginsHere,
    TryModifyingThisPattern,
}

impl Display for InfoKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InfoKind::DelimitedExpressionBeginsHere => "delimited expression begins here",
                InfoKind::TryModifyingThisPattern => "try modifying this pattern",
            }
        )
    }
}
