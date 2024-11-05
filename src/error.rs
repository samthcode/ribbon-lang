//! This module contains the necessary objects to define Ribbon errors.
//!
//! For example, an invalid operator (an opertor which is not defined in Ribbon, eg ^) would be assigned to the enum variant [InvalidOperator](ErrorKind::InvalidOperator)

use crate::{
    lexer::token::{LiteralKind, TokenKind},
    pos::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }
}

pub fn eprint_error_message(errors: Vec<Error>, file_name: &str, source: &str) {
    eprintln!(
        "File {file_name} failed to compile with {} error{}:",
        errors.len(),
        if errors.len() == 1 { "" } else { "s" }
    );

    for error in errors {
        eprintln!();
        eprintln!("error: {file_name}:{}: {}", error.span.start, error.kind);
        if error.span.start.line == error.span.end.line {
            let col_len = error.span.start.line.to_string().len();
            eprintln!("{} |", " ".repeat(col_len));
            eprintln!(
                "{} | {}",
                error.span.start.line,
                source
                    .lines()
                    .nth(error.span.start.line - 1)
                    .unwrap_or_else(|| panic!(
                        "Ribbon internal error printing errors! Whoops! Kinda ironic lol"
                    ))
            );
            eprintln!(
                "{} | {}{}",
                " ".repeat(col_len),
                " ".repeat(error.span.start.col - 1),
                "^".repeat(std::cmp::max(1, error.span.end.col - error.span.start.col))
            );
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    ExpectedXFoundY(char, char),
    ExpectedXFoundEof(char),
    UnexpectedCharacter(char),
    InvalidOperator(String),
    InvalidLiteral(LiteralKind),
    EofWhileLexingLiteral(LiteralKind),
    InvalidEscapeCharacter(char, LiteralKind),
    EofWhileParsing,
    ExpectedEndOfExpressionFoundX(TokenKind),
    ExpectedTokenFoundOther(TokenKind, Option<TokenKind>),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorKind::*;
        write!(
            f,
            "{}",
            match self {
                ExpectedXFoundY(expected, found) =>
                    format!(r#"Expected '{expected}', found '{found}'"#),
                ExpectedXFoundEof(expected) => format!(r#"Expected '{expected}', found EOF"#),
                UnexpectedCharacter(ch) => format!(r#"Unexpected character '{ch}'"#),
                InvalidOperator(op) => format!(r#"Invalid operator '{op}'"#),
                InvalidLiteral(literal_type) => format!(
                    r#"Invalid{} literal"#,
                    literal_kind_to_string(literal_type.clone())
                ),
                EofWhileLexingLiteral(literal_type) => format!(
                    r#"EOF while lexing {} literal"#,
                    literal_kind_to_string(literal_type.clone())
                ),
                InvalidEscapeCharacter(ch, literal) => format!(
                    r#"Invalid escape character '\{ch}' in {} literal"#,
                    literal_kind_to_string(literal.clone())
                ),
                EofWhileParsing => String::from("EOF While parsing"),
                ExpectedEndOfExpressionFoundX(n) =>
                    format!(r#"Expected end of expression, found: {:?}"#, n),
                ExpectedTokenFoundOther(t, o) => {
                    if o.is_none() {
                        format!(r#"Expected {t:?} found EOF"#)
                    } else {
                        format!(r#"Expected {t:?} found {:?}"#, o.clone().unwrap())
                    }
                }
            }
        )
    }
}

fn literal_kind_to_string<'a>(literal_kind: LiteralKind) -> &'a str {
    match literal_kind {
        LiteralKind::Str(_) => " string",
        LiteralKind::Char(_) => " character",
        LiteralKind::Float(_) => " float",
        _ => "",
    }
}
