use crate::pos::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl Error {
    pub fn new(span: Span, message: String) -> Self {
        Self { span, message }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR: {}: {}", self.span, self.message)
    }
}

pub fn eprint_error_message(errors: Vec<Error>, file_name: &str, source: String) {
    eprintln!("File {file_name} failed to compile with {} error{}:", errors.len(), if errors.len() == 1 {""} else {"s"});

    for error in errors {
        eprintln!();
        eprintln!("error: {file_name}:{}: {}", error.span.start, error.message);
        if error.span.start.line == error.span.end.line {
            let col_len = error.span.start.line.to_string().len();
            eprintln!("{} |", " ".repeat(col_len));
            eprintln!("{} | {}", error.span.start.line, source.lines().nth(error.span.start.line - 1).unwrap_or_else(|| panic!("Ribbon internal error printing errors! Whoops! Kinda ironic lol")));
            eprintln!("{} | {}{}", " ".repeat(col_len), " ".repeat(error.span.start.col - 1), "^".repeat(std::cmp::max(1, error.span.end.col - error.span.start.col)));
        }
    }
}