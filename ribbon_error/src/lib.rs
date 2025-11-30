use std::{error::Error, fmt::Display};

use ribbon_lexer::{TokKind, span::Span};

#[derive(Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    span: Span,
    subdiagnostics: Vec<Diagnostic>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, span: Span) -> Self {
        Self {
            kind,
            span,
            subdiagnostics: vec![],
        }
    }

    pub fn new_error(kind: ErrorKind, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Error(kind),
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

    pub fn with_subdiagnostics(self, subdiagnostics: Vec<Diagnostic>) -> Self {
        Self {
            subdiagnostics,
            ..self
        }
    }
}

impl Display for Diagnostic {
    // TODO: This is temporary
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.span, self.kind)?;
        for sub in &self.subdiagnostics {
            write!(f, "\n{}", sub)?
        }
        write!(f, "")
    }
}

impl Error for Diagnostic {}

#[derive(Debug)]
pub enum DiagnosticKind {
    Error(ErrorKind),
    Warning(WarningKind),
    Info(InfoKind),
}

impl Display for DiagnosticKind {
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
pub enum ErrorKind {
    ExpectedXFoundY(TokKind, TokKind),
    ExpectedOneOfXFoundY(Vec<TokKind>, TokKind),
    UnclosedDelimitedExpression,
    UnexpectedToken(TokKind),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ErrorKind::ExpectedXFoundY(expected, found) =>
                    format!("expected {expected}, found {found}"),
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
                ErrorKind::UnexpectedToken(tok) => format!("unexpected token {}", tok),
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
