use std::fmt::Display;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Span,
}

impl Tok {
    pub fn new(kind: TokKind, span: Span) -> Self {
        Tok { kind, span }
    }
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}-{}] ", self.span.low, self.span.hi)?;
        write!(
            f,
            "{}",
            match &self.kind {
                TokKind::Ident(i) => format!("ident:{}", *i),
                TokKind::Kw(kw) => format!("kw:{}", kw.str()),
                TokKind::Lit(lit) => format!("lit:{}", lit.string()),
                TokKind::Op(op) => format!("op:\"{}\"", op.str()),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum KwKind {
    /// `const`
    Const,
    /// `struct`
    Struct,
    /// `trait`
    Trait,
    /// `enum`
    Enum,
    /// `return`
    Return,
    /// `use`
    Use,
    /// `for`
    For,
    /// `while`
    While,
}

impl KwKind {
    fn str(&self) -> &str {
        use KwKind::*;
        match self {
            Const => "const",
            Struct => "struct",
            Trait => "trait",
            Enum => "enum",
            Return => "return",
            Use => "use",
            For => "for",
            While => "while",
        }
    }
}

impl Display for KwKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "kw:{}", self.str())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InvalidStrKind {
    Unclosed,
    UnterminatedEscape,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
    /// e.g. `42`
    Int(i64),
    /// e.g. `"Hello World!"`
    Str(Box<String>),
    /// e.g. `"Hello World`
    /// This is given to the parser so that it can provide better error messages
    InvalidStr(Box<String>, InvalidStrKind),
    /// e.g. `42.0`
    Float(f64),
    /// true or false
    Bool(bool),
}

impl LitKind {
    fn string(&self) -> String {
        use LitKind::*;
        match self {
            Int(i) => format!("{}", i),
            Str(s) => format!("\"{}\"", *s),
            InvalidStr(s, k) => format!(
                "\"{}\"({})",
                *s,
                match k {
                    InvalidStrKind::Unclosed => "unclosed",
                    InvalidStrKind::UnterminatedEscape => "unterminated escape",
                }
            ),
            Float(f) => format!("{}", f),
            Bool(b) => format!("{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpKind {
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
    /// `^`
    Caret,
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
    /// `^=`
    CaretEq,
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
    /// `->`
    MinusGt,
    /// `=>`
    EqGt,

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

    /// `..=`
    DotDotEq,
}

impl OpKind {
    pub fn str(&self) -> &str {
        use OpKind::*;
        match &self {
            Plus => "+",
            Minus => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            LParen => "(",
            RParen => ")",
            LSquare => "[",
            RSquare => "]",
            LCurly => "{",
            RCurly => "}",
            Dot => ".",
            Colon => ":",
            Semi => ";",
            At => "@",
            Hash => "#",
            Tilde => "~",
            Amp => "&",
            Caret => "^",
            Pipe => "|",
            Bang => "!",
            Eq => "=",
            Dollar => "$",
            Lt => "<",
            Gt => ">",
            EqEq => "==",
            BangEq => "!=",
            LtEq => "<=",
            GtEq => ">=",
            AmpEq => "&=",
            CaretEq => "^=",
            PipeEq => "|=",
            PlusEq => "+=",
            MinusEq => "-=",
            MulEq => "*=",
            DivEq => "/=",
            ModEq => "%=",
            DotEq => ".=",
            And => "&&",
            Or => "||",
            Path => "::",
            DotDot => "..",
            ColonGt => ":>",
            TildeGt => "~>",
            TildeQuestion => "~?",
            ShiftL => "<<",
            ShiftR => ">>",
            AndEq => "&&=",
            OrEq => "||=",
            ShiftLEq => "<<=",
            ShiftREq => ">>=",
            DotDotEq => "..=",
            MinusGt => "->",
            EqGt => "=>",
        }
    }

    pub fn try_expand(&self, c: &char) -> Option<Self> {
        use OpKind::*;
        match (self, *c) {
            (Plus, '=') => Some(PlusEq),
            (Minus, '=') => Some(MinusEq),
            (Minus, '>') => Some(MinusGt),
            (Mul, '=') => Some(MulEq),
            (Div, '=') => Some(DivEq),
            (Mod, '=') => Some(ModEq),
            (Dot, '=') => Some(DotEq),
            (Dot, '.') => Some(DotDot),
            (Colon, '>') => Some(ColonGt),
            (Tilde, '>') => Some(TildeGt),
            (Tilde, '?') => Some(TildeQuestion),
            (Amp, '=') => Some(AmpEq),
            (Amp, '&') => Some(And),
            (Caret, '=') => Some(CaretEq),
            (Pipe, '=') => Some(PipeEq),
            (Pipe, '|') => Some(Or),
            (Bang, '=') => Some(BangEq),
            (Eq, '=') => Some(EqEq),
            (Eq, '>') => Some(EqGt),
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

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    Ident(Box<String>),

    Kw(KwKind),

    Lit(LitKind),

    Op(OpKind),
}

impl TokKind {
    pub fn maybe_ident_to_kw_or_bool(self) -> Self {
        use KwKind::*;
        use TokKind::*;
        if let Ident(ref str) = self {
            match (*str).as_str() {
                "const" => Kw(Const),
                "struct" => Kw(Struct),
                "trait" => Kw(Trait),
                "enum" => Kw(Enum),
                "return" => Kw(Return),
                "use" => Kw(Use),
                "for" => Kw(For),
                "while" => Kw(While),
                "true" => Lit(LitKind::Bool(true)),
                "false" => Lit(LitKind::Bool(false)),
                _ => self,
            }
        } else {
            panic!("Called maybe_ident_to_kw_or_bool on non-identifier.")
        }
    }
}

macro_rules! tok {
    ($tok_kind:ident, $start:expr$(, $end:expr)?) => {
        tok::Tok::new(tok::TokKind::$tok_kind, ($start$(, $end)?).into())
    };

    (Ident($str:expr), $start:expr$(, $end:expr)?) => {
        tok::Tok::new(tok::TokKind::Ident(Box::new($str)), ($start$(, $end)?).into())
    };
    (@maybe_conv Ident($str:expr), $start:expr$(, $end:expr)?) => {
        tok::Tok::new(
            tok::TokKind::Ident(Box::new($str)).maybe_ident_to_kw_or_bool(),
            ($start$(, $end)?).into(),
        )
    };
    (Lit(Str($str:expr)), $start:expr$(, $end:expr)?) => {
        tok::Tok::new(tok::TokKind::Lit(tok::LitKind::Str(Box::new($str))), ($start$(, $end)?).into())
    };
    (Lit(InvalidStr($str:expr, $kind:ident)), $start:expr$(, $end:expr)?) => {
        tok::Tok::new(tok::TokKind::Lit(tok::LitKind::InvalidStr(Box::new($str), tok::InvalidStrKind::$kind)), ($start$(, $end)?).into())
    };
    ($tok_kind:ident($e:expr), $start:expr$(, $end:expr)?) => {
        tok::Tok::new(TokKind::$tok_kind($e), ($start$(, $end)?).into())
    }
}
