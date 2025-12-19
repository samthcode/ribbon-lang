use ribbon_error::{Diagnostic, ErrorKind};
use ribbon_lexer::{OpKind, span::Span};
use serde::Serialize;

use crate::Expr;

#[derive(Debug, Clone, Serialize)]
pub struct BinOp {
    lhs: Expr,
    rhs: Expr,
    kind: BinOpKind,
    op_span: Span,
}

impl BinOp {
    pub fn new(lhs: Expr, rhs: Expr, kind: BinOpKind, op_span: Span) -> Self {
        Self {
            lhs,
            rhs,
            kind,
            op_span,
        }
    }

    pub fn try_from_op_kind<'a>(
        op_kind: OpKind,
        op_span: Span,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<Self, Diagnostic<'a>> {
        Ok(Self {
            lhs,
            rhs,
            kind: op_kind.try_into().map_err(|_| {
                Diagnostic::new_error(ErrorKind::InvalidBinaryOperator(op_kind), op_span)
            })?,
            op_span,
        })
    }

    pub fn sexpr(&self) -> String {
        format!(
            "({} {} {})",
            self.kind.str(),
            self.lhs.sexpr(),
            self.rhs.sexpr()
        )
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum BinOpKind {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    /// `<`
    Lt,
    // `>`
    Gt,
    /// `==`
    Equality,
    /// `!=`
    Inequality,
    /// `<=`
    LtEquality,
    /// `>=`
    GtEquality,

    /// `&`
    BwAnd,
    /// `^`
    BwXor,
    /// `|`
    BwOr,

    /// `=`
    Assign,
    /// `&=`
    BwAndAssign,
    /// `^=`
    BwXorAssign,
    /// `|=`
    BwOrAssign,
    /// `+=`
    AddAssign,
    /// -=
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    ModAssign,
    /// `.=`
    DotAssign,

    /// `&&`
    LogicalAnd,
    /// `||`
    LogicalOr,
    /// `:>`
    MethodPipe,
    /// `~>`
    NonMethodPipe,

    /// `~?`
    ErrProp,

    /// `<<`
    ShiftL,
    /// `>>`
    ShiftR,

    /// `&&=`
    LogicalAndAssign,
    /// `||=`
    LogicalOrAssign,

    /// `<<=`
    ShiftLAssign,
    /// `>>=`
    ShiftRAssign,

    /// `->`
    ThinArrow,
    /// `=>`
    FatArrow,

    /// A (strictly) parenthesised method call
    MethodParen,
    /// Method call with no required parentheses
    MethodNoParen,
    /// Non-method called with `~` & method conventions
    NonMethod,

    /// `::`
    Path,
    /// `..`
    RangeExclusive,
    /// `..=`
    RangeInclusive,
}

impl BinOpKind {
    pub fn str(&self) -> &str {
        match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Mod => "%",
            BinOpKind::Lt => "<",
            BinOpKind::Gt => ">",
            BinOpKind::Equality => "==",
            BinOpKind::Inequality => "!=",
            BinOpKind::LtEquality => "<=",
            BinOpKind::GtEquality => ">=",
            BinOpKind::BwAnd => "&",
            BinOpKind::BwXor => "^",
            BinOpKind::BwOr => "|",
            BinOpKind::Assign => "=",
            BinOpKind::BwAndAssign => "&=",
            BinOpKind::BwXorAssign => "^=",
            BinOpKind::BwOrAssign => "|=",
            BinOpKind::AddAssign => "+=",
            BinOpKind::SubAssign => "-=",
            BinOpKind::MulAssign => "*=",
            BinOpKind::DivAssign => "/=",
            BinOpKind::ModAssign => "%=",
            BinOpKind::DotAssign => ".=",
            BinOpKind::LogicalAnd => "&&",
            BinOpKind::LogicalOr => "||",
            BinOpKind::MethodPipe => ":>",
            BinOpKind::NonMethodPipe => "~>",
            BinOpKind::ErrProp => "~?",
            BinOpKind::ShiftL => "<<",
            BinOpKind::ShiftR => ">>",
            BinOpKind::LogicalAndAssign => "&&=",
            BinOpKind::LogicalOrAssign => "||=",
            BinOpKind::ShiftLAssign => "<<=",
            BinOpKind::ShiftRAssign => ">>=",
            BinOpKind::ThinArrow => "->",
            BinOpKind::FatArrow => "=>",
            BinOpKind::MethodParen => ".",
            BinOpKind::MethodNoParen => ":",
            BinOpKind::NonMethod => "~",
            BinOpKind::Path => "::",
            BinOpKind::RangeExclusive => "..",
            BinOpKind::RangeInclusive => "..=",
        }
    }
}

impl TryFrom<OpKind> for BinOpKind {
    type Error = ();
    fn try_from(kind: OpKind) -> Result<Self, Self::Error> {
        use BinOpKind::*;
        match kind {
            OpKind::Plus => Ok(Add),
            OpKind::Minus => Ok(Sub),
            OpKind::Mul => Ok(Mul),
            OpKind::Div => Ok(Div),
            OpKind::Mod => Ok(Mod),
            OpKind::Amp => Ok(BwAnd),
            OpKind::Caret => Ok(BwXor),
            OpKind::Pipe => Ok(BwOr),
            OpKind::Lt => Ok(Lt),
            OpKind::Gt => Ok(Gt),
            OpKind::EqEq => Ok(Equality),
            OpKind::BangEq => Ok(Inequality),
            OpKind::LtEq => Ok(LtEquality),
            OpKind::GtEq => Ok(GtEquality),
            OpKind::Eq => Ok(Assign),
            OpKind::AmpEq => Ok(BwAndAssign),
            OpKind::CaretEq => Ok(BwXorAssign),
            OpKind::PipeEq => Ok(BwOrAssign),
            OpKind::PlusEq => Ok(AddAssign),
            OpKind::MinusEq => Ok(SubAssign),
            OpKind::MulEq => Ok(MulAssign),
            OpKind::DivEq => Ok(DivAssign),
            OpKind::ModEq => Ok(ModAssign),
            OpKind::DotEq => Ok(DotAssign),
            OpKind::And => Ok(LogicalAnd),
            OpKind::Or => Ok(LogicalOr),
            OpKind::ColonGt => Ok(MethodPipe),
            OpKind::TildeGt => Ok(NonMethodPipe),
            OpKind::MinusGt => Ok(ThinArrow),
            OpKind::EqGt => Ok(FatArrow),
            OpKind::TildeQuestion => Ok(ErrProp),
            OpKind::ShiftL => Ok(ShiftL),
            OpKind::ShiftR => Ok(ShiftR),
            OpKind::AndEq => Ok(LogicalAndAssign),
            OpKind::OrEq => Ok(LogicalOrAssign),
            OpKind::ShiftLEq => Ok(ShiftLAssign),
            OpKind::ShiftREq => Ok(ShiftRAssign),
            OpKind::Dot => Ok(MethodParen),
            OpKind::Colon => Ok(MethodNoParen),
            OpKind::Tilde => Ok(NonMethod),
            OpKind::Path => Ok(Path),
            OpKind::DotDot => Ok(RangeExclusive),
            OpKind::DotDotEq => Ok(RangeInclusive),
            _ => Err(()),
        }
    }
}
