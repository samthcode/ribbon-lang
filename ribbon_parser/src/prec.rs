use crate::lexer;
use lexer::tok::TokKind;

pub fn unary_prec(kind: &TokKind) -> u8 {
    match kind {
        TokKind::Ident(_) => todo!(),
        TokKind::Kw(kw_kind) => todo!(),
        TokKind::Lit(lit_kind) => todo!(),
        TokKind::Op(op_kind) => todo!(),
    }
}

pub fn binary_prec(kind: &TokKind) -> u8 {
    use lexer::OpKind::*;
    match kind {
        TokKind::Ident(_) => todo!(),
        TokKind::Kw(kw_kind) => todo!(),
        TokKind::Lit(lit_kind) => todo!(),
        TokKind::Op(op_kind) => match op_kind {
            Plus | Minus => todo!(),
            Mul | Div => todo!(),
            Mod => todo!(),
            LParen => todo!(),
            RParen => todo!(),
            LSquare => todo!(),
            RSquare => todo!(),
            LCurly => todo!(),
            RCurly => todo!(),
            Dot => todo!(),
            Colon => todo!(),
            Semi => todo!(),
            At => todo!(),
            Hash => todo!(),
            Tilde => todo!(),
            Amp => todo!(),
            Pipe => todo!(),
            Bang => todo!(),
            Eq | AmpEq | PipeEq | PlusEq | MinusEq | MulEq | DivEq | ModEq | DotEq | ShiftLEq
            | ShiftREq => todo!(),
            Dollar => todo!(),
            Lt => todo!(),
            Gt => todo!(),
            EqEq => todo!(),
            BangEq => todo!(),
            LtEq => todo!(),
            GtEq => todo!(),
            And => todo!(),
            Or => todo!(),
            Path => todo!(),
            DotDot => todo!(),
            ColonGt => todo!(),
            TildeGt => todo!(),
            MinusGt => todo!(),
            EqGt => todo!(),
            TildeQuestion => todo!(),
            ShiftL => todo!(),
            ShiftR => todo!(),
            AndEq => todo!(),
            OrEq => todo!(),
            DotDotEq => todo!(),
        },
    }
}
