use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Tok {
    kind: TokKind,
    span: Span,
}

impl Tok {
    pub fn new(kind: TokKind, span: Span) -> Self {
        Tok { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    Ident(Box<String>),

    // Keywords
    /// `const`
    KwConst,
    /// `struct`
    KwStruct,
    /// `enum`
    KwEnum,
    /// `return`
    KwReturn,
    /// `use`
    KwUse,
    /// `for`
    KwFor,
    /// `while`
    KwWhile,

    // Literals
    /// e.g. `42`
    LitInt(i64),
    /// e.g. `"Hello World!"`
    LitStr(Box<String>),
    /// e.g. `42.0`
    LitFloat(i64),

    // Single-Character Operators
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LSquare,
    /// `]`
    RSquare,
    /// `{`
    LCurly,
    /// `}`
    RCurly,

    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `;`
    Semi,
    /// `@`
    At,
    /// `#`
    Hash,
    /// `~`
    Tilde,

    /// `&`
    Amp,
    /// `|`
    Pipe,

    /// `!`
    Bang,

    /// `=`
    Eq,

    /// `$`
    Dollar,

    // Two-Character Operators
    /// `==`
    EqEq,
    /// `!=`
    BangEq,

    /// `&=`
    AmpEq,
    /// `|=`
    PipeEq,
    /// `+=`
    PusEq,
    /// -=
    MinusEq,
    /// `*=`
    MulEq,
    /// `/=`
    DivEq,
    /// `.=`
    DotEq,

    /// `&&`
    And,
    /// `||`
    Or,

    /// `::`
    Path,
    /// `..`
    DotDot,

    /// `:>`
    ColonGt,
    /// `~>`
    TildeGt,

    /// `<<`
    ShiftL,
    /// `>>`
    ShiftR,

    // Three-Character Operators
    /// `&&=`
    AndEq,
    /// `||=`
    OrEq,

    /// `<<=`
    ShiftLEq,
    /// `>>=`
    ShiftREq,
}

macro_rules! tok {
    ($tok_kind:ident, $start:expr) => {
        Tok::new(TokKind::$tok_kind, ($start, $start).into())
    };
    (Ident($str:expr), $start:expr) => {
        Tok::new(TokKind::Ident(Box::new($str)), ($start, $start).into())
    };
    ($tok_kind:ident($e:expr), $start:expr) => {
        Tok::new(TokKind::$tok_kind($e), ($start, $start).into())
    };

    ($tok_kind:ident, $start:expr, $end:expr) => {
        Tok::new(TokKind::$tok_kind, ($start, $end).into())
    };
    (Ident($str:expr), $start:expr, $end:expr) => {
        Tok::new(TokKind::Ident(Box::new($str)), ($start, $end).into())
    };
    ($tok_kind:ident($e:expr), $start:expr, $end:expr) => {
        Tok::new(TokKind::$tok_kind($e), ($start, $end).into())
    };
}
