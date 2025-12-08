use super::*;

use tok::{Tok, TokKind::*};

macro_rules! test {
    ($source:literal$(,($tok:expr, $low:expr, $hi:expr))*) => {
        assert_eq!(Lexer::new($source).into_iter().collect::<Vec<Tok>>(), vec![
            $(Tok::new($tok,&$source[$low..=$hi], ($low, $hi).into())),*,
            Tok::eof(if $source.len() == 0 {0} else {$source.len() - 1})
        ])
    };
    ($source:literal, $($tok:expr),*) => {
        assert_eq!(Lexer::new($source).into_iter().collect::<Vec<Tok>>(), vec![
            $($tok,)*
            Tok::eof(if $source.len() == 0 {0} else {$source.len() - 1})
        ])
    }
}

#[test]
fn ident() {
    test!("hello", Tok::new(Ident, "hello", Span::new(0, 4)));
    test!("_hello", Tok::new(Ident, "_hello", Span::new(0, 5)));
    test!("t1_var_?", Tok::new(Ident, "t1_var_?", Span::new(0, 7)));
}

#[test]
fn operators() {
    use OpKind::*;
    test!(
        "+=+-/~?<<=",
        Tok::new(Op(PlusEq), "+=", (0, 1).into()),
        Tok::new(Op(Plus), "+", 2.into()),
        Tok::new(Op(Minus), "-", 3.into()),
        Tok::new(Op(Div), "/", 4.into()),
        Tok::new(Op(TildeQuestion), "~?", (5, 6).into()),
        Tok::new(Op(ShiftLEq), "<<=", (7, 9).into())
    );
    test!("^", Tok::new(Op(Caret), "^", 0.into()));
    test!("^=", Tok::new(Op(CaretEq), "^=", (0, 1).into()))
}

#[test]
fn whitespace() {
    test!(" 123", Tok::new(Lit(LitKind::Int), "123", (1, 3).into()));
    test!("     ident", Tok::new(Ident, "ident", (5, 9).into()));
    test!(
        "     ident ()",
        Tok::new(Ident, "ident", (5, 9).into()),
        Tok::new(Op(OpKind::LParen), "(", 11.into()),
        Tok::new(Op(OpKind::RParen), ")", 12.into())
    );
    test!("     \n\r\tident", Tok::new(Ident, "ident", (8, 12).into()));
    test!(
        "!ident + 10",
        Tok::new(Op(OpKind::Bang), "!", (0, 0).into()),
        Tok::new(Ident, "ident", (1, 5).into()),
        Tok::new(Op(OpKind::Plus), "+", (7, 7).into()),
        Tok::new(Lit(LitKind::Int), "10", (9, 10).into())
    )
}

#[test]
fn strings() {
    test!(
        "\"Hello World\"",
        Tok::new(
            Lit(LitKind::UnprocessedStr),
            "\"Hello World\"",
            (0, 12).into()
        )
    );
    test!(
        "\"Hello World",
        Tok::new(
            Lit(LitKind::InvalidStr(InvalidStrKind::Unclosed)),
            "\"Hello World",
            (0, 11).into()
        )
    );
    test!(
        "\"\\t\"",
        Tok::new(Lit(LitKind::UnprocessedStr), "\"\\t\"", (0, 3).into())
    )
}

#[test]
fn bools() {
    test!("true", Tok::new(Lit(LitKind::Bool), "true", (0, 3).into()));
    test!(
        "false",
        Tok::new(Lit(LitKind::Bool), "false", (0, 4).into())
    )
}

#[test]
fn numbers() {
    test!("100", (Lit(LitKind::Int), 0, 2));
    test!(
        "100..",
        (Lit(LitKind::Int), 0, 2),
        (Op(OpKind::DotDot), 3, 4)
    );
    test!("100.1", (Lit(LitKind::Float), 0, 4));
    test!(
        "100.func",
        (Lit(LitKind::Int), 0, 2),
        (Op(OpKind::Dot), 3, 3),
        (Ident, 4, 7)
    );
    test!("1.", (Lit(LitKind::Float), 0, 1));
    test!("100_000_000", (Lit(LitKind::Int), 0, 10));
    test!("9", (Lit(LitKind::Int), 0, 0));
    test!("9876543210", (Lit(LitKind::Int), 0, 9))
}

#[test]
fn line_comments() {
    test!("//hello this is a comment\r\n",);
    test!(
        "//hello this is a comment\r\n101",
        (Lit(LitKind::Int), 27, 29)
    )
}

#[test]
fn block_comments() {
    test!("/*block comment*/",);
    test!("/*block*comment*/",);
    test!("hello/**/123", (Ident, 0, 4), (Lit(LitKind::Int), 9, 11));
    test!("hello/***/123", (Ident, 0, 4), (Lit(LitKind::Int), 10, 12));
    test!(
        "hello/*****/123",
        (Ident, 0, 4),
        (Lit(LitKind::Int), 12, 14)
    );
    test!("hello/*****123*/", (Ident, 0, 4));
    test!(
        "/*block comment*/\n123 // Line comment\n /**/ 123",
        (Lit(LitKind::Int), 18, 20),
        (Lit(LitKind::Int), 44, 46)
    );
}

#[test]
fn empty_source() {
    test!("",)
}
