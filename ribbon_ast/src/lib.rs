use ribbon_error::{Diagnostic, ErrorKind};
use ribbon_lexer::{OpKind, Tok, TokKind, span::Span};

/// The root AST node for a Ribbon program
///
/// This contains a list of expressions which make up all of the functions, structs, enums, and
/// constants at the top level of a program, as well as all of the expressions contained within those expressions.
#[derive(Debug)]
pub struct Program<'a> {
    pub body: Vec<Expr>,
    pub diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> Default for Program<'a> {
    fn default() -> Self {
        Self {
            body: vec![],
            diagnostics: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }

    pub fn is(&self, other: ExprKind) -> bool {
        self.kind.is(other)
    }
}

impl Expr {
    pub fn sexpr(&self) -> String {
        self.kind.sexpr()
    }
}

#[derive(Debug, Clone)]
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
    TupleOrParameterList(Vec<Expr>),
    // This is only needed to correctly parse function declarations with a single parameter
    // (and no trailing comma inside the parameter list)
    ParenthesisedExpression(Box<Expr>),
    Tuple(Vec<Expr>),
    FunctionDeclaration {
        parameters: Vec<Expr>,
        generic_parameters: Vec<Expr>,
        arrow_span: Span,
        return_type: Box<Expr>,
        body: Vec<Expr>,
    },
    UnitType,
    Block(Vec<Expr>),
    /// Represents an invalid portion of code
    Invalid,
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
                    "(list{}{})",
                    if elems.is_empty() { "" } else { " " },
                    space_sexprs(elems, " ")
                )
            }
            ExprKind::TupleOrParameterList(exprs) => {
                format!(
                    "(tuple-param-list{}{})",
                    if exprs.is_empty() { "" } else { " " },
                    space_sexprs(exprs, " ")
                )
            }
            ExprKind::ParenthesisedExpression(expr) => expr.sexpr(),
            ExprKind::Tuple(exprs) => {
                format!(
                    "(tuple{}{})",
                    if exprs.is_empty() { "" } else { " " },
                    space_sexprs(exprs, " ")
                )
            }
            ExprKind::FunctionDeclaration {
                parameters,
                generic_parameters,
                arrow_span: _,
                return_type,
                body,
            } => format!(
                "(fn {}(params{}) (ret {}) {})",
                if generic_parameters.len() > 0 {
                    format!("(generics {}) ", space_sexprs(generic_parameters, " "))
                } else {
                    "".to_string()
                },
                format!(
                    "{}{}",
                    if parameters.len() > 0 { " " } else { "" },
                    space_sexprs(parameters, " ")
                ),
                return_type.sexpr(),
                if body.len() > 0 {
                    format!("(body\n    {}\n)", space_sexprs(body, "\n    "))
                } else {
                    "(body)".to_string()
                }
            ),
            ExprKind::UnitType => "()".to_string(),
            ExprKind::Block(exprs) => {
                if exprs.len() == 0 {
                    return "(block)".to_string();
                }
                format!("(block\n    {}\n)", space_sexprs(exprs, "\n    "))
            }
            ExprKind::Invalid => "(<invalid>)".to_string(),
        }
    }

    pub fn is(&self, other: Self) -> bool {
        match self {
            ExprKind::BinOp { .. } => matches!(other, ExprKind::BinOp { .. }),
            ExprKind::UnaryOp { .. } => matches!(other, ExprKind::UnaryOp { .. }),
            ExprKind::Ident(_) => matches!(other, ExprKind::Ident(_)),
            ExprKind::Lit(_) => matches!(other, ExprKind::Lit(_)),
            ExprKind::List(_) => matches!(other, ExprKind::List(_)),
            ExprKind::TupleOrParameterList(_) => matches!(other, ExprKind::TupleOrParameterList(_)),
            ExprKind::ParenthesisedExpression(_) => {
                matches!(other, ExprKind::ParenthesisedExpression(_))
            }
            ExprKind::Tuple(_) => matches!(other, ExprKind::Tuple(_)),
            ExprKind::FunctionDeclaration { .. } => {
                matches!(other, ExprKind::FunctionDeclaration { .. })
            }
            ExprKind::UnitType => matches!(other, ExprKind::UnitType),
            ExprKind::Block(_) => matches!(other, ExprKind::Block(_)),
            ExprKind::Invalid => matches!(other, ExprKind::Invalid),
        }
    }
}

fn space_sexprs(exprs: &Vec<Expr>, sep: &str) -> String {
    exprs
        .iter()
        .map(|e| e.sexpr())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

pub fn tok_to_expr(tok: Tok) -> Result<Expr, Diagnostic> {
    use ExprKind::*;
    use ribbon_lexer::LitKind as LLK;
    Ok(Expr::new(
        match tok.kind {
            TokKind::Ident => Ident(Box::new(tok.source.to_string())),
            TokKind::Lit(kind) => match kind {
                // TODO: At some point, we need to find the true size of the integer
                LLK::Int => Lit(LitKind::Int(
                    tok.source
                        .replace("_", "")
                        .parse()
                        .expect("internal: failed to parse int literal"),
                )),
                LLK::UnprocessedStr => Lit(LitKind::Str(Box::new(process_str(tok.source)))),
                LLK::InvalidStr(invalid_str_kind) => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::InvalidStringLiteral(invalid_str_kind),
                        tok.span,
                    ));
                }
                LLK::Float => Lit(LitKind::Float(
                    tok.source
                        .replace("_", "")
                        .parse()
                        .expect("internal: failed to parse float literal"),
                )),
                LLK::Bool => Lit(LitKind::Bool(
                    tok.source
                        .parse()
                        .expect("interal: failed to parse boolean literal"),
                )),
            },
            _ => {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(tok),
                    tok.span,
                ));
            }
        },
        tok.span,
    ))
}

pub fn process_str(source: &str) -> String {
    let mut res = String::new();
    let mut chars = source.chars();
    // Skip the starting `"`
    chars.next();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                return res;
            }
            '\\' => match chars.next() {
                Some('\\') => res.push('\\'),
                Some('"') => res.push('"'),
                Some('n') => res.push('\n'),
                Some('r') => res.push('\r'),
                Some('t') => res.push('\t'),
                Some('0') => res.push('\0'),
                None => {
                    panic!("called `process_str` on invalid string literal")
                }
                _ => res.push(c),
            },
            _ => res.push(c),
        }
    }
    panic!("called `process_str` on invalid string literal")
}
