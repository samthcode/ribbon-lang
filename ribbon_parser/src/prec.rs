use crate::lexer;
use lexer::tok::TokKind;

pub enum Fixity {
    Left,
    Right,
    None,
}

pub fn unary_prec(kind: &TokKind) -> u8 {
    use lexer::OpKind::*;
    match kind {
        TokKind::Ident(_) => todo!(),
        TokKind::Kw(_) => todo!(),
        TokKind::Lit(_) => todo!(),
        TokKind::Op(op_kind) => match op_kind {
            // Unary minus/not/deref/borrow
            Minus | Bang | Mul | Amp => 15,
            // This shouldn't be called on anything else
            // Need an error system set up in the parser first
            _ => todo!(),
        },
    }
}

fn binary_prec(kind: &TokKind) -> (u8, Fixity) {
    use Fixity::*;
    use lexer::OpKind::*;
    match kind {
        TokKind::Op(op_kind) => match op_kind {
            Path => (19, None),
            // Method call/field expression/no-parenthesis method call/(non-method) tilde call
            Dot | Colon | Tilde => (18, Left),
            // Error propagation
            TildeQuestion => (17, Left),
            // Function call/array indexing
            LParen | LSquare => (16, None),
            // -- Gap for unary ops --
            Mul | Div | Mod => (14, Left),
            Plus | Minus => (13, Left),
            ShiftL | ShiftR => (12, Left),
            Amp => (11, Left),
            Caret => (10, Left),
            Pipe => (9, Left),
            // Comparisons
            Lt | Gt | EqEq | BangEq | LtEq | GtEq => (8, Left),
            And => (7, Left),
            Or => (6, Left),
            // Ranges
            DotDot | DotDotEq => (5, None),
            // Piping operators
            ColonGt | TildeGt => (4, Left),
            // All assignments
            Eq | AmpEq | CaretEq | PipeEq | PlusEq | MinusEq | MulEq | DivEq | ModEq | DotEq
            | ShiftLEq | ShiftREq | AndEq | OrEq => (3, Right),
            // Block
            LCurly | RCurly => (2, None),
            // Argument list terminator
            RParen => (1, None),
            // List terminator
            RSquare => (0, None),

            // Not sure what to do with these yet
            Semi => todo!(),
            At => todo!(),
            Hash => todo!(),
            Bang => todo!(),
            Dollar => todo!(),
            MinusGt => todo!(),
            EqGt => todo!(),
        },
        // binary_prec shouldn't be called with these afaik
        TokKind::Ident(_) => todo!(),
        TokKind::Kw(_) => todo!(),
        TokKind::Lit(_) => todo!(),
    }
}
