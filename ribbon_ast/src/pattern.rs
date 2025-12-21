use serde::Serialize;

#[derive(Debug, Clone, Serialize, Default)]
pub struct Pattern {
    kind: PatternKind,
}

impl Pattern {
    pub fn new(kind: PatternKind) -> Self {
        Self { kind }
    }

    pub fn sexpr(&self) -> String {
        match &self.kind {
            PatternKind::Identifier(ident) => format!("(pattern {ident})"),
            PatternKind::Literal => todo!(),
            PatternKind::Wildcard => todo!(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Default)]
pub enum PatternKind {
    /// `ident`
    Identifier(String),
    Literal,
    #[default]
    /// `_`
    Wildcard,
    // ...
}
