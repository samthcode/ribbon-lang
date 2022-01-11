use crate::pos::Span;

#[derive(Debug, Clone)]
pub struct RibbonError {
    span: Span,
    message: String
}

impl RibbonError {
    pub fn new(span: Span, message: String) -> Self {
        Self {
            span,
            message
        }
    }
}

impl std::fmt::Display for RibbonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.span, self.message)
    }
}