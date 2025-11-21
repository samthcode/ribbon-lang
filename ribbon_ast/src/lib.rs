use ribbon_lexer::{LitKind, Tok, TokKind, span::Span};

/// The root AST node for a Ribbon program
///
/// This contains a list of expressions which make up all of the functions, structs, enums, and
/// constants at the top level of a program, as well as all of the expressions contained within those expressions.
pub struct Program {
    pub body: Vec<Expr>,
}

impl Default for Program {
    fn default() -> Self {
        Self { body: vec![] }
    }
}

pub struct Expr {
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

pub enum ExprKind {
    BinOp {
        lhs: Box<ExprKind>,
        rhs: Box<ExprKind>,
        kind: BinOpKind,
    },
    Ident(Box<String>),
}

pub enum BinOpKind {}

pub fn tok_to_expr(tok: Tok) -> Option<Expr> {
    match tok.kind {
        TokKind::Ident(s) => Some(Expr::new(ExprKind::Ident(s), tok.span)),
        _ => todo!(),
    }
}
