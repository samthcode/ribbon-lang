use ribbon_lexer::{OpKind, span::Span};

use crate::Expr;

#[derive(Debug, Clone)]
pub struct UnaryOp {
    kind: UnaryOpKind,
    op_span: Span,
    rhs: Expr,
}

impl UnaryOp {
    pub fn new(kind: UnaryOpKind, op_span: Span, rhs: Expr) -> Self {
        Self { kind, op_span, rhs }
    }

    pub fn str(&self) -> &str {
        self.kind.str()
    }

    pub fn sexpr(&self) -> String {
        format!("({} {})", self.kind.str(), self.rhs.sexpr())
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Minus,
    Not,
    Deref,
    Ref,
}

impl UnaryOpKind {
    pub fn str(&self) -> &str {
        match self {
            UnaryOpKind::Minus => "-",
            UnaryOpKind::Not => "!",
            UnaryOpKind::Deref => "*",
            UnaryOpKind::Ref => "&",
        }
    }
}

impl TryFrom<OpKind> for UnaryOpKind {
    type Error = ();

    fn try_from(value: OpKind) -> Result<Self, Self::Error> {
        match value {
            OpKind::Minus => Ok(UnaryOpKind::Minus),
            OpKind::Bang => Ok(UnaryOpKind::Not),
            OpKind::Mul => Ok(UnaryOpKind::Deref),
            OpKind::Amp => Ok(UnaryOpKind::Ref),
            // There's no convenient way of adding the span here
            _ => Err(()),
        }
    }
}
