use crate::lexer;
use lexer::tok::TokKind;
use ribbon_lexer::OpKind;

pub enum Fixity {
    Left,
    Right,
    None,
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub enum Prec {
    Semi,
    ListTerminator,
    ArgTerminator,
    Block,
    Assign,
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

pub fn unary_prec(kind: OpKind) -> Prec {
    use lexer::OpKind::*;
    match kind {
        // Unary minus/not/deref/borrow
        Minus | Bang | Mul | Amp => Prec::Unary,
        // This shouldn't be called on anything else
        // Need an error system set up in the parser first
        _ => todo!(),
    }
}

pub fn binary_prec(kind: OpKind) -> (Prec, Fixity) {
    use Fixity::*;
    use lexer::OpKind::*;
    match kind {
        Path => (Prec::Path, None),
        // Method call/field expression/no-parenthesis method call/(non-method) tilde call
        Dot | Colon | Tilde => (Prec::MethodCall, Left),
        // Error propagation
        TildeQuestion => (Prec::ErrProp, Left),
        // Function call/array indexing
        LParen | LSquare => (Prec::FunctionCall, None),
        // -- Gap for unary ops --
        Mul | Div | Mod => (Prec::Mul, Left),
        Plus | Minus => (Prec::Add, Left),
        ShiftL | ShiftR => (Prec::Shift, Left),
        Amp => (Prec::BwAnd, Left),
        Caret => (Prec::BwXor, Left),
        Pipe => (Prec::BwOr, Left),
        // Comparisons
        Lt | Gt | EqEq | BangEq | LtEq | GtEq => (Prec::Comparison, Left),
        And => (Prec::And, Left),
        Or => (Prec::Or, Left),
        // Ranges
        DotDot | DotDotEq => (Prec::Range, None),
        // Piping operators
        ColonGt | TildeGt => (Prec::Pipe, Left),
        // All assignments
        Eq | AmpEq | CaretEq | PipeEq | PlusEq | MinusEq | MulEq | DivEq | ModEq | DotEq
        | ShiftLEq | ShiftREq | AndEq | OrEq => (Prec::Assign, Right),
        // Block
        LCurly | RCurly => (Prec::Block, None),
        // Argument list terminator
        RParen => (Prec::ArgTerminator, None),
        // List terminator
        RSquare => (Prec::ListTerminator, None),
        // Semicolon
        Semi => (Prec::Semi, None),

        // Not sure what to do with these yet
        At => todo!(),
        Hash => todo!(),
        Bang => todo!(),
        Dollar => todo!(),
        MinusGt => todo!(),
        EqGt => todo!(),
    }
}
