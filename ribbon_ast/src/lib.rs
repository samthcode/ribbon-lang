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

pub enum Expr {
    BinOp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: BinOpKind,
    },
}

pub enum BinOpKind {}
