use std::cmp::Ordering;

use ribbon_lexer::{OpKind, TokKind};

#[derive(Debug, PartialEq, Eq)]
pub enum Fixity {
    Left,
    Right,
    None,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum PrecOrd {
    Block,
    Semi,
    ListTerminator,
    ArgTerminator,
    ListSeparator,
    Assign,
    FunctionDef,
    Pipe,
    Range,
    Or,
    And,
    Comparison,
    BwOr,
    BwXor,
    BwAnd,
    Shift,
    Add,
    Mul,
    Unary,
    FunctionCall,
    ErrProp,
    MethodCall,
    Path,
}

pub fn unary_prec(kind: &OpKind) -> Prec {
    use ribbon_lexer::OpKind::*;
    Prec::new(
        match kind {
            // Unary minus/not/deref/borrow
            Minus | Bang | Mul | Amp => PrecOrd::Unary,
            // This shouldn't be called on anything else
            // Need an error system set up in the parser first
            _ => todo!(),
        },
        Fixity::None,
    )
}

#[derive(Debug, Eq, PartialEq)]
pub struct Prec {
    prec_ord: PrecOrd,
    fixity: Fixity,
}

impl Prec {
    pub fn new(prec_ord: PrecOrd, fixity: Fixity) -> Self {
        Prec { prec_ord, fixity }
    }
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match self.prec_ord.cmp(&other.prec_ord) {
            ord @ (Ordering::Greater | Ordering::Less) => ord,
            Ordering::Equal => match self.fixity {
                Fixity::Left => match other.fixity {
                    Fixity::Right | Fixity::None => Ordering::Equal,
                    Fixity::Left => Ordering::Less,
                },
                Fixity::Right => match other.fixity {
                    Fixity::Right | Fixity::None => Ordering::Greater,
                    Fixity::Left => Ordering::Equal,
                },
                Fixity::None => match other.fixity {
                    Fixity::Left => Ordering::Less,
                    Fixity::Right | Fixity::None => Ordering::Equal,
                },
            },
        })
    }
}

pub fn binary_prec(kind: TokKind) -> Option<Prec> {
    match kind {
        TokKind::Op(op_kind) => Some(binary_op_prec(op_kind)),
        // A keyword may some day have need of a precedence, such as `as` or `if`
        TokKind::Kw(_) | TokKind::Ident | TokKind::Lit(_) | TokKind::Eof => None,
        TokKind::Dummy => panic!("internal: called `binary_prec` on dummy token"),
    }
}

pub fn binary_op_prec(kind: OpKind) -> Prec {
    use Fixity::*;
    use ribbon_lexer::OpKind::*;
    match kind {
        Path => Prec::new(PrecOrd::Path, None),
        // Method call/field expression/no-parenthesis method call/(non-method) tilde call
        Dot | Colon | Tilde => Prec::new(PrecOrd::MethodCall, Left),
        // Error propagation
        TildeQuestion => Prec::new(PrecOrd::ErrProp, Left),
        // Function call/array indexing
        LParen | LSquare => Prec::new(PrecOrd::FunctionCall, None),
        // -- Gap for unary ops --
        Mul | Div | Mod => Prec::new(PrecOrd::Mul, Left),
        Plus | Minus => Prec::new(PrecOrd::Add, Left),
        ShiftL | ShiftR => Prec::new(PrecOrd::Shift, Left),
        Amp => Prec::new(PrecOrd::BwAnd, Left),
        Caret => Prec::new(PrecOrd::BwXor, Left),
        Pipe => Prec::new(PrecOrd::BwOr, Left),
        // Comparisons
        Lt | Gt | EqEq | BangEq | LtEq | GtEq => Prec::new(PrecOrd::Comparison, Left),
        And => Prec::new(PrecOrd::And, Left),
        Or => Prec::new(PrecOrd::Or, Left),
        // Ranges
        DotDot | DotDotEq => Prec::new(PrecOrd::Range, None),
        // Piping operators
        ColonGt | TildeGt => Prec::new(PrecOrd::Pipe, Left),
        // All assignments
        Eq | AmpEq | CaretEq | PipeEq | PlusEq | MinusEq | MulEq | DivEq | ModEq | DotEq
        | ShiftLEq | ShiftREq | AndEq | OrEq => Prec::new(PrecOrd::Assign, Right),
        // Function definition
        MinusGt | EqGt => Prec::new(PrecOrd::FunctionDef, Fixity::None),
        // List separator
        Comma => Prec::new(PrecOrd::ListSeparator, Fixity::None),
        // Argument list terminator
        RParen => Prec::new(PrecOrd::ArgTerminator, None),
        // List terminator
        RSquare => Prec::new(PrecOrd::ListTerminator, None),
        // Semicolon
        Semi => Prec::new(PrecOrd::Semi, None),
        // Block
        LCurly | RCurly => Prec::new(PrecOrd::Block, None),

        // Not sure what to do with these yet
        At => todo!(),
        Hash => todo!(),
        Bang => todo!(),
        Dollar => todo!(),
    }
}
