use std::{error::Error, fmt::format};

use ribbon_lexer::{OpKind, Tok, TokKind, span::Span};

/// The root AST node for a Ribbon program
///
/// This contains a list of expressions which make up all of the functions, structs, enums, and
/// constants at the top level of a program, as well as all of the expressions contained within those expressions.
#[derive(Debug)]
pub struct Program {
    pub body: Vec<Expr>,
}

impl Default for Program {
    fn default() -> Self {
        Self { body: vec![] }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

impl Expr {
    pub fn sexpr(&self) -> String {
        self.kind.sexpr()
    }
}

#[derive(Debug)]
pub enum ExprKind {
    BinOp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: BinOp,
    },
    UnaryOp {
        rhs: Box<Expr>,
        kind: UnaryOp,
    },
    Ident(Box<String>),
    Lit(LitKind),
    List(Vec<Expr>),
}

impl ExprKind {
    pub fn sexpr(&self) -> String {
        match self {
            ExprKind::BinOp { lhs, rhs, kind } => {
                format!("({} {} {})", kind.str(), lhs.sexpr(), rhs.sexpr())
            }
            ExprKind::UnaryOp { rhs, kind } => {
                format!("({} {})", kind.str(), rhs.sexpr())
            }
            ExprKind::Ident(s) => s.to_string(),
            ExprKind::Lit(lit_kind) => lit_kind.repr(),
            ExprKind::List(elems) => {
                format!(
                    "(list {})",
                    elems
                        .iter()
                        .map(|e| e.sexpr())
                        .reduce(|acc, elem| format!("{acc} {elem}"))
                        .unwrap()
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum LitKind {
    /// e.g. `42`
    Int(i64),
    /// e.g. `"Hello World!"`
    Str(Box<String>),
    /// e.g. `42.0`
    Float(f64),
    /// true or false
    Bool(bool),
}

impl LitKind {
    pub fn repr(&self) -> String {
        match self {
            LitKind::Int(i) => i.to_string(),
            LitKind::Str(s) => format!("\"{s}\""),
            LitKind::Float(f) => f.to_string(),
            LitKind::Bool(b) => b.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct BinOp {
    kind: BinOpKind,
    span: Span,
}

impl BinOp {
    pub fn new(kind: BinOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn str(&self) -> &str {
        self.kind.str()
    }
}

#[derive(Debug)]
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
    // Special cases ---
    // These will have their own expr kinds because they don't conform specifically to LHS and RHS
    // /// `!`
    // ErrUnion,
    // /// `=`
    // Assign,
    // /// `::`
    // Path,
    // /// `..`
    // RangeExclusive,
    // /// `..=`
    // RangeInclusive,
    // -----------------
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
        }
    }
}

impl TryFrom<OpKind> for BinOpKind {
    type Error = Box<dyn Error>;
    fn try_from(kind: OpKind) -> Result<Self, Box<dyn Error>> {
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
            // TODO: Proper error type
            _ => Err("Unexpected operator.".into()),
        }
    }
}

#[derive(Debug)]
pub struct UnaryOp {
    kind: UnaryOpKind,
    span: Span,
}

impl UnaryOp {
    pub fn new(kind: UnaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn str(&self) -> &str {
        self.kind.str()
    }
}

#[derive(Debug)]
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
    type Error = Box<dyn Error>;

    fn try_from(value: OpKind) -> Result<Self, Self::Error> {
        match value {
            OpKind::Minus => Ok(UnaryOpKind::Minus),
            OpKind::Bang => Ok(UnaryOpKind::Not),
            OpKind::Mul => Ok(UnaryOpKind::Deref),
            OpKind::Amp => Ok(UnaryOpKind::Ref),
            _ => Err(format!("Unexpected operator {}.", value.str()).into()),
        }
    }
}

pub fn tok_to_expr(tok: Tok) -> Result<Expr, Box<dyn Error>> {
    use ExprKind::*;
    use ribbon_lexer::LitKind as LLK;
    let kind = match tok.kind {
        TokKind::Ident(s) => Some(Ident(s)),
        TokKind::Lit(kind) => match kind {
            // TODO: At some point, we need to find the true size of the integer
            LLK::Int(i) => Some(Lit(LitKind::Int(i))),
            LLK::Str(s) => Some(Lit(LitKind::Str(s))),
            // TODO: Proper error type
            LLK::InvalidStr(_, invalid_str_kind) => return Err("Invalid string literal.".into()),
            LLK::Float(f) => Some(Lit(LitKind::Float(f))),
            LLK::Bool(b) => Some(Lit(LitKind::Bool(b))),
        },
        _ => todo!(),
    };
    if let Some(k) = kind {
        Ok(Expr::new(k, tok.span))
    } else {
        // TODO: Proper error message
        Err("Unexpected token.".into())
    }
}
