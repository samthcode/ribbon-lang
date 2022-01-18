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
