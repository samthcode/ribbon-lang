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
    /// `trait`
    KwTrait,
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

    /// `<`
    Lt,
    // `>`
    Gt,

    // Two-Character Operators
    /// `==`
    EqEq,
    /// `!=`
    BangEq,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,

    /// `&=`
    AmpEq,
    /// `|=`
    PipeEq,
    /// `+=`
    PlusEq,
    /// -=
    MinusEq,
    /// `*=`
    MulEq,
    /// `/=`
    DivEq,
    /// `%=`
    ModEq,
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

    /// `~?`
    TildeQuestion,

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

    /// `DotDotEq`
    DotDotEq,
}

impl TokKind {
    pub fn maybe_ident_to_kw(self) -> Self {
        use TokKind::*;
        if let Ident(ref str) = self {
            match (*str).as_str() {
                "const" => KwConst,
                "struct" => KwStruct,
                "trait" => KwTrait,
                "enum" => KwEnum,
                "return" => KwReturn,
                "use" => KwUse,
                "for" => KwFor,
                "while" => KwWhile,
                _ => self,
            }
        } else {
            panic!("Called maybe_ident_to_kw on non-identifier.")
        }
    }

    pub fn try_expand_op(&self, c: &char) -> Option<Self> {
        use TokKind::*;
        match (self, *c) {
            (Plus, '=') => Some(PlusEq),
            (Minus, '=') => Some(MinusEq),
            (Mul, '=') => Some(MulEq),
            (Div, '=') => Some(DivEq),
            (Mod, '=') => Some(ModEq),
            (Dot, '=') => Some(DotEq),
            (Colon, '>') => Some(ColonGt),
            (Tilde, '>') => Some(TildeGt),
            (Tilde, '?') => Some(TildeQuestion),
            (Amp, '=') => Some(AmpEq),
            (Amp, '&') => Some(And),
            (Pipe, '=') => Some(PipeEq),
            (Pipe, '|') => Some(Or),
            (Bang, '=') => Some(BangEq),
            (Eq, '=') => Some(EqEq),
            (Lt, '=') => Some(LtEq),
            (Lt, '<') => Some(ShiftL),
            (Gt, '=') => Some(GtEq),
            (Gt, '>') => Some(ShiftR),
            // To Three-Character
            (And, '=') => Some(AndEq),
            (Or, '=') => Some(OrEq),
            (DotDot, '=') => Some(DotDotEq),
            (ShiftL, '=') => Some(ShiftLEq),
            (ShiftR, '=') => Some(ShiftREq),
            _ => None,
        }
    }
}

macro_rules! tok {
    ($tok_kind:ident, $start:expr) => {
        Tok::new(TokKind::$tok_kind, ($start, $start).into())
    };
    (Ident($str:expr), $start:expr) => {
        Tok::new(TokKind::Ident(Box::new($str)), ($start, $start).into())
    };
    (@maybe_kw Ident($str:expr), $start:expr) => {
        Tok::new(
            TokKind::Ident(Box::new($str)).maybe_ident_to_kw(),
            ($start, $start).into(),
        )
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
    (@maybe_kw Ident($str:expr), $start:expr, $end:expr) => {
        Tok::new(
            TokKind::Ident(Box::new($str)).maybe_ident_to_kw(),
            ($start, $end).into(),
        )
    };
    ($tok_kind:ident($e:expr), $start:expr, $end:expr) => {
        Tok::new(TokKind::$tok_kind($e), ($start, $end).into())
    };
}
