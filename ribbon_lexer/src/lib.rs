mod char_util;
mod cursor;
pub mod lexer;
pub mod span;
#[cfg(test)]
mod test;
pub mod tok;

pub use lexer::*;
pub use tok::*;

#[macro_export]
macro_rules! op {
    (+) => {
        OpKind::Plus
    };
    (-) => {
        OpKind::Minus
    };
    (*) => {
        OpKind::Mul
    };
    (/) => {
        OpKind::Div
    };
    (%) => {
        OpKind::Mod
    };
    ("(") => {
        OpKind::LParen
    };
    (")") => {
        OpKind::RParen
    };
    ("[") => {
        OpKind::LSquare
    };
    ("]") => {
        OpKind::RSquare
    };
    ("{") => {
        OpKind::LCurly
    };
    ("}") => {
        OpKind::RCurly
    };
    (,) => {
        OpKind::Comma
    };
    (.) => {
        OpKind::Dot
    };
    (:) => {
        OpKind::Colon
    };
    (;) => {
        OpKind::Semi
    };
    (@) => {
        OpKind::At
    };
    (#) => {
        OpKind::Hash
    };
    (~) => {
        OpKind::Tilde
    };
    (&) => {
        OpKind::Amp
    };
    (^) => {
        OpKind::Caret
    };
    (|) => {
        OpKind::Pipe
    };
    (!) => {
        OpKind::Bang
    };
    (=) => {
        OpKind::Eq
    };
    ($) => {
        OpKind::Dollar
    };
    (<) => {
        OpKind::Lt
    };
    (>) => {
        OpKind::Gt
    };
    (==) => {
        OpKind::EqEq
    };
    (!=) => {
        OpKind::BangEq
    };
    (<=) => {
        OpKind::LtEq
    };
    (>=) => {
        OpKind::GtEq
    };
    (&=) => {
        OpKind::AmpEq
    };
    (^=) => {
        OpKind::CaretEq
    };
    (|=) => {
        OpKind::PipeEq
    };
    (+=) => {
        OpKind::PlusEq
    };
    (-=) => {
        OpKind::MinusEq
    };
    (*=) => {
        OpKind::MulEq
    };
    (/=) => {
        OpKind::DivEq
    };
    (%=) => {
        OpKind::ModEq
    };
    (.=) => {
        OpKind::DotEq
    };
    (&&) => {
        OpKind::And
    };
    (||) => {
        OpKind::Or
    };
    (::) => {
        OpKind::Path
    };
    (..) => {
        OpKind::DotDot
    };
    (:>) => {
        OpKind::ColonGt
    };
    (~>) => {
        OpKind::TildeGt
    };
    (~?) => {
        OpKind::TildeQuestion
    };
    (<<) => {
        OpKind::ShiftL
    };
    (>>) => {
        OpKind::ShiftR
    };
    (&&=) => {
        OpKind::AndEq
    };
    (||=) => {
        OpKind::OrEq
    };
    (<<=) => {
        OpKind::ShiftLEq
    };
    (>>=) => {
        OpKind::ShiftREq
    };
    (..=) => {
        OpKind::DotDotEq
    };
    (->) => {
        OpKind::MinusGt
    };
    (=>) => {
        OpKind::EqGt
    };
}

#[macro_export]
macro_rules! kw {
    (const) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Const)
    };
    (struct) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Struct)
    };
    (trait) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Trait)
    };
    (enum) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Enum)
    };
    (return) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Return)
    };
    (use) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::Use)
    };
    (for) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::For)
    };
    (while) => {
        TokKind::Kw(ribbon_lexer::tok::KwKind::While)
    };
}
