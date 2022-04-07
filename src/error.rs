use crate::{lexer::token::LiteralKind, pos::Span};

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

pub fn eprint_error_message(errors: Vec<Error>, file_name: &str, source: String) {
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
    ExpectedXFoundEOF(char),
    UnexpectedCharacter(char),
    InvalidOperator(String),
    InvalidLiteral(LiteralKind),
    EOFWhileLexingLiteral(LiteralKind),
    InvalidEscapeCharacter(char, LiteralKind),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::ExpectedXFoundY(expected, found) =>
                    format!(r#"Expected '{expected}', found '{found}'"#),
                Self::ExpectedXFoundEOF(expected) => format!(r#"Expected '{expected}', found EOF"#),
                Self::UnexpectedCharacter(ch) => format!(r#"Unexpected character '{ch}'"#),
                Self::InvalidOperator(op) => format!(r#"Invalid operator '{op}'"#),
                Self::InvalidLiteral(literal_type) => format!(
                    r#"Invalid{} literal"#,
                    literal_kind_to_string(literal_type.clone())
                ),
                Self::EOFWhileLexingLiteral(literal_type) => format!(
                    r#"EOF while lexing {} literal"#,
                    literal_kind_to_string(literal_type.clone())
                ),
                Self::InvalidEscapeCharacter(ch, literal) =>
                    format!(r#"Invalid escape character '\{ch}' in {} literal"#, literal_kind_to_string(literal.clone())),
            }
        )
    }
}

fn literal_kind_to_string<'a>(literal_kind: LiteralKind) -> &'a str {
    match literal_kind {
        LiteralKind::String(_) => " string",
        LiteralKind::Char(_) => " character",
        LiteralKind::Float(_) => " float",
        _ => "",
    }
}
