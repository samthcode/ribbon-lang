use std::fmt::Display;

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

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}-{}] ", self.span.low, self.span.hi)?;
        let res = match &self.kind {
            TokKind::Ident(i) => format!("ident:{}", *i),
            TokKind::KwConst => "kw:const".to_string(),
            TokKind::KwStruct => "kw:struct".to_string(),
            TokKind::KwTrait => "kw:trait".to_string(),
            TokKind::KwEnum => "kw:enum".to_string(),
            TokKind::KwReturn => "kw:return".to_string(),
            TokKind::KwUse => "kw:use".to_string(),
            TokKind::KwFor => "kw:for".to_string(),
            TokKind::KwWhile => "kw:while".to_string(),
            TokKind::LitInt(i) => format!("int:{}", i),
            TokKind::LitStr(s) => format!("str:{}", s),
            TokKind::LitInvalidStr(s, kind) => match kind {
                LitInvalidStrKind::Unclosed => format!("invalid-str:{}(unclosed)", s),
                LitInvalidStrKind::UnterminatedEscape => {
                    format!("invalid-str:{}(unterminated escape", s)
                }
            },
            TokKind::LitFloat(f) => format!("float:{}", f),
            TokKind::LitBool(b) => format!("bool:{}", b),
            operator => format!("op:\"{}\"", operator.op_symbol())
        };
        writeln!(f, "{}", res)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitInvalidStrKind {
    Unclosed,
    UnterminatedEscape,
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
    /// e.g. `"Hello World`
    /// This is given to the parser so that it can provide better error messages
    LitInvalidStr(Box<String>, LitInvalidStrKind),
    /// e.g. `42.0`
    LitFloat(f64),
    /// true or false
    LitBool(bool),

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
    pub fn maybe_ident_to_kw_or_bool(self) -> Self {
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
                "true" => LitBool(true),
                "false" => LitBool(false),
                _ => self,
            }
        } else {
            panic!("Called maybe_ident_to_kw_or_bool on non-identifier.")
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

    pub fn op_symbol(&self) -> &str {
        match &self {
            TokKind::Plus => "+",
            TokKind::Minus => "-",
            TokKind::Mul => "*",
            TokKind::Div => "/",
            TokKind::Mod => "%",
            TokKind::LParen => "(",
            TokKind::RParen => ")",
            TokKind::LSquare => "[",
            TokKind::RSquare => "]",
            TokKind::LCurly => "{",
            TokKind::RCurly => "}",
            TokKind::Dot => ".",
            TokKind::Colon => ":",
            TokKind::Semi => ";",
            TokKind::At => "@",
            TokKind::Hash => "#",
            TokKind::Tilde => "~",
            TokKind::Amp => "&",
            TokKind::Pipe => "|",
            TokKind::Bang => "!",
            TokKind::Eq => "=",
            TokKind::Dollar => "$",
            TokKind::Lt => "<",
            TokKind::Gt => ">",
            TokKind::EqEq => "==",
            TokKind::BangEq => "!=",
            TokKind::LtEq => "<=",
            TokKind::GtEq => ">=",
            TokKind::AmpEq => "&=",
            TokKind::PipeEq => "|=",
            TokKind::PlusEq => "+=",
            TokKind::MinusEq => "-=",
            TokKind::MulEq => "*=",
            TokKind::DivEq => "/=",
            TokKind::ModEq => "%=",
            TokKind::DotEq => ".=",
            TokKind::And => "&&",
            TokKind::Or => "||",
            TokKind::Path => "::",
            TokKind::DotDot => "..",
            TokKind::ColonGt => ":>",
            TokKind::TildeGt => "~>",
            TokKind::TildeQuestion => "~?",
            TokKind::ShiftL => "<<",
            TokKind::ShiftR => ">>",
            TokKind::AndEq => "&&=",
            TokKind::OrEq => "||=",
            TokKind::ShiftLEq => "<<=",
            TokKind::ShiftREq => ">>=",
            TokKind::DotDotEq => "..=",
            _ => panic!("Called op_symbol on non-operator")
        }
    }
}

macro_rules! tok {
    ($tok_kind:ident, $start:expr$(, $end:expr)?) => {
        Tok::new(TokKind::$tok_kind, ($start$(, $end)?).into())
    };

    (Ident($str:expr), $start:expr$(, $end:expr)?) => {
        Tok::new(TokKind::Ident(Box::new($str)), ($start$(, $end)?).into())
    };
    (@maybe_conv Ident($str:expr), $start:expr$(, $end:expr)?) => {
        Tok::new(
            TokKind::Ident(Box::new($str)).maybe_ident_to_kw_or_bool(),
            ($start$(, $end)?).into(),
        )
    };
    (LitStr($str:expr), $start:expr$(, $end:expr)?) => {
        Tok::new(TokKind::LitStr(Box::new($str)), ($start$(, $end)?).into())
    };
    (LitInvalidStr($str:expr, $kind:ident), $start:expr$(, $end:expr)?) => {
        Tok::new(TokKind::LitInvalidStr(Box::new($str), tok::LitInvalidStrKind::$kind), ($start$(, $end)?).into())
    };
    ($tok_kind:ident($e:expr), $start:expr$(, $end:expr)?) => {
        Tok::new(TokKind::$tok_kind($e), ($start$(, $end)?).into())
    }
}
