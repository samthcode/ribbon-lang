use std::fmt::Display;

use ribbon_lexer::{OpKind, Tok, TokKind, span::Span, tok::InvalidStrKind};

#[derive(Debug)]
pub struct Diagnostic<'a> {
    kind: DiagnosticKind<'a>,
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ErrorKind<'a> {
    ExpectedXFoundY(TokKind, Tok<'a>),
    ExpectedOneOfXFoundY(Vec<TokKind>, Tok<'a>),
    UnclosedDelimitedExpression,
    UnexpectedToken(Tok<'a>),
    UnexpectedOperator(OpKind),
    InvalidBinaryOperator(OpKind),
    UnexpectedEofAfterBinaryOperator,
    InvalidStringLiteral(InvalidStrKind),
    UnexpectedDelimiter(OpKind),
    UnexpectedEofAfterUnaryOperator,
}

impl<'a> Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // FIXME: Replace use of Debug
                ErrorKind::ExpectedXFoundY(expected, found) =>
                    format!("expected {expected:?}, found {found}"),
                ErrorKind::ExpectedOneOfXFoundY(expected, found) => {
                    format!(
                        "expected one of {}, found {}",
                        expected
                            .iter()
                            // FIXME: Replace use of Debug
                            .map(|e| format!("{e:?}"))
                            .reduce(|acc, e| format!("{acc}, {e}"))
                            .unwrap(),
                        found
                    )
                }
                ErrorKind::UnclosedDelimitedExpression =>
                    "unclosed delimited expression".to_string(),
                ErrorKind::UnexpectedToken(tok) => format!("unexpected token {}", tok),
                ErrorKind::UnexpectedOperator(op) => format!("unexpected operator {}", op.str()),
                ErrorKind::InvalidBinaryOperator(op) =>
                    format!("invalid binary operator {}", op.str()),
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
            }
        )
    }
}

#[derive(Debug)]
pub enum WarningKind {}

impl Display for WarningKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug)]
pub enum InfoKind {
    DelimitedExpressionBeginsHere,
}

impl Display for InfoKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InfoKind::DelimitedExpressionBeginsHere => "delimited expression begins here",
            }
        )
    }
}
