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
    KwConst,  // `const`
    KwStruct, // `struct`
    KwEnum,   // `enum`
    KwReturn, // `return`
    KwUse,    // `use`
    KwFor,    // `for`
    KwWhile,  // `while`

    // Literals
    LitInt(i64),         // e.g. `42`
    LitStr(Box<String>), // e.g. `"Hello World!"`
    LitFloat(i64),       // e.g. `42.0`

    // Single-Character Operators
    Plus,  // `+`
    Minus, // `-`
    Mul,   // `*`
    Div,   // `/`
    Mod,   // `%`

    LParen,  // `(`
    RParen,  // `)`
    LSquare, // `[`
    RSquare, // `]`
    LCurly,  // `{`
    RCurly,  // `}`

    Dot,   // `.`
    Colon, // `:`
    Semi,  // `;`
    At,    // `@`
    Hash,  // `#`
    Tilde, // `~`

    BitAnd, // `&`
    BitOr,  // `|`

    Not, // `!`

    Eq, // `=`

    Dollar, // `$`

    // Multi-Character Operators
    EqEq,  // `==`
    NotEq, // `!=`

    AndEq,   // `&=`
    OrEq,    // `|=`
    PusEq,   // `+=`
    MinusEq, // -=
    MulEq,   // `*=`
    DivEq,   // `/=`
    DotEq,   // `.=`

    And, // `&&`
    Or,  // `||`

    Path,  // `::`
    Range, // `..` or `..=`

    ColonGt, // `:>`
    TildeGt, // `~>`
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
