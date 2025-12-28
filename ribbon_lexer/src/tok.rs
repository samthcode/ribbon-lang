use std::fmt::Display;

use serde::Serialize;

use crate::span::Span;

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Tok<'a> {
    pub kind: TokKind,
    pub source: &'a str,
    pub span: Span,
}

impl<'a> Tok<'a> {
    pub fn new(kind: TokKind, source: &'a str, span: Span) -> Self {
        Tok { kind, source, span }
    }

    pub fn dummy() -> Self {
        Self {
            kind: TokKind::Dummy,
            source: "",
            span: Span::default(),
        }
    }

    pub fn eof(pos: usize) -> Self {
        Self {
            kind: TokKind::Eof,
            source: "",
            span: pos.into(),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind.is_eof()
    }

    pub fn is_op(&self) -> bool {
        self.kind.is_op()
    }

    pub fn is_op_kind(&self, kind: OpKind) -> bool {
        self.kind.is_op_kind(kind)
    }

    pub fn is_kw_kind(&self, kind: KwKind) -> bool {
        self.kind.is_kw(kind)
    }

    pub fn maybe_ident_to_kw_or_bool(&mut self) -> Self {
        use KwKind::*;
        use TokKind::*;
        if self.kind == Ident {
            Self {
                kind: match self.source {
                    "const" => Kw(Const),
                    "struct" => Kw(Struct),
                    "trait" => Kw(Trait),
                    "enum" => Kw(Enum),
                    "return" => Kw(Return),
                    "use" => Kw(Use),
                    "for" => Kw(For),
                    "while" => Kw(While),
                    "true" => Lit(LitKind::Bool),
                    "false" => Lit(LitKind::Bool),
                    _ => self.kind,
                },
                source: self.source,
                span: self.span,
            }
        } else {
            panic!("called `maybe_ident_to_kw_or_bool` on non-identifier")
        }
    }
}

impl<'a> Display for Tok<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            if self.kind.is_eof() {
                "EOF"
            } else {
                self.source
            }
        )
    }
}

// If adding a new keyword, please consider updating the `kw!` macro in `ribbon_lexer/lib.rs`
#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
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

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum InvalidStrKind {
    Unclosed,
    UnterminatedEscape,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum LitKind {
    /// e.g. `42`
    ///       ^^
    Int,
    /// e.g. `"Hello\n World!"`
    ///       ^^^^^^^^^^^^^^^^
    /// Represents the string as it is in the source code. Does not process escape characters etc.
    UnprocessedStr,
    /// e.g. `"Hello World`
    ///       ^^^^^^^^^^^^
    /// This is given to the parser so that it can provide better error messages
    InvalidStr(InvalidStrKind),
    /// e.g. `42.0`
    ///       ^^^^
    Float,
    /// `true` or `false`
    ///  ^^^^      ^^^^^
    Bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
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

    /// `,`
    Comma,

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
    /// `>`
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
    /// `-=`
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
            Comma => ",",
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
            (Colon, ':') => Some(Path),
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

    pub fn is_delim(&self) -> bool {
        use OpKind::*;
        matches!(self, LCurly | RCurly | LParen | RParen | LSquare | RSquare)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum TokKind {
    Ident,

    Kw(KwKind),

    Lit(LitKind),

    Op(OpKind),

    Eof,

    /// For internal use only
    Dummy,
}

impl TokKind {
    pub fn is_kw(&self, kind: KwKind) -> bool {
        matches!(self, TokKind::Kw(k) if k == &kind)
    }

    pub fn is_op(&self) -> bool {
        matches!(self, TokKind::Op(_))
    }

    pub fn is_op_kind(&self, other: OpKind) -> bool {
        match self {
            TokKind::Op(op_kind) => op_kind == &other,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        self == &TokKind::Eof
    }
}

impl Display for TokKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokKind::Ident => "ident".to_string(),
                TokKind::Kw(kw) => format!("kw:`{}`", kw.str()),
                TokKind::Lit(_) => "lit".to_string(),
                TokKind::Op(op) => format!("`{}`", op.str()),
                TokKind::Eof => "EOF".to_string(),
                TokKind::Dummy => panic!("internal: tried to display dummy token"),
            }
        )
    }
}
