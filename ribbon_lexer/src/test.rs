use super::*;

macro_rules! test {
    ($source:literal, $($tok:expr),* $(,)?) => {
        assert_eq!(Lexer::new($source).into_iter().collect::<Vec<Tok>>(), vec![
            $($tok,)*
            tok!(Eof, if $source.len() == 0 {0} else {$source.len() as u32 - 1})
        ])
    }
}

#[test]
fn ident() {
    test!("hello", tok!(Ident("hello".to_string()), 0, 4));
    test!("_hello", tok!(Ident("_hello".to_string()), 0, 5));
    test!("t1_var_?", tok!(Ident("t1_var_?".to_string()), 0, 7));
}

#[test]
fn operators() {
    use OpKind::*;
    test!(
        "+=+-/~?<<=",
        tok!(Op(PlusEq), 0, 1),
        tok!(Op(Plus), 2),
        tok!(Op(Minus), 3),
        tok!(Op(Div), 4),
        tok!(Op(TildeQuestion), 5, 6),
        tok!(Op(ShiftLEq), 7, 9)
    );
    test!("^", tok!(Op(Caret), 0));
    test!("^=", tok!(Op(CaretEq), 0, 1))
}

#[test]
fn whitespace() {
    test!(" 123", tok!(Lit(LitKind::Int(123)), 1, 3));
    test!("     ident", tok!(Ident("ident".to_string()), 5, 9));
    test!(
        "     ident ()",
        tok!(Ident("ident".to_string()), 5, 9),
        tok!(Op(OpKind::LParen), 11),
        tok!(Op(OpKind::RParen), 12)
    );
    test!("     \n\r\tident", tok!(Ident("ident".to_string()), 8, 12));
    test!(
        "!ident + 10",
        tok!(Op(OpKind::Bang), 0, 0),
        tok!(Ident("ident".to_string()), 1, 5),
        tok!(Op(OpKind::Plus), 7, 7),
        tok!(Lit(LitKind::Int(10)), 9, 10)
    )
}

#[test]
fn strings() {
    test!(
        "\"Hello World\"",
        tok!(Lit(Str("Hello World".to_string())), 0, 12)
    );
    test!(
        "\"Hello World",
        tok!(Lit(InvalidStr("Hello World".to_string(), Unclosed)), 0, 11)
    );
    test!("\"\\t\"", tok!(Lit(Str("\t".to_string())), 0, 3))
}

#[test]
fn bools() {
    test!("true", tok!(Lit(LitKind::Bool(true)), 0, 3));
    test!("false", tok!(Lit(LitKind::Bool(false)), 0, 4))
}

#[test]
fn numbers() {
    test!("100", tok!(Lit(LitKind::Int(100)), 0, 2));
    test!(
        "100..",
        tok!(Lit(LitKind::Int(100)), 0, 2),
        tok!(Op(OpKind::DotDot), 3, 4)
    );
    test!("100.1", tok!(Lit(LitKind::Float(100.1)), 0, 4));
    test!(
        "100.func",
        tok!(Lit(LitKind::Int(100)), 0, 2),
        tok!(Op(OpKind::Dot), 3),
        tok!(Ident("func".to_string()), 4, 7)
    );
    test!("1.", tok!(Lit(LitKind::Float(1.0)), 0, 1));
    test!("100_000_000", tok!(Lit(LitKind::Int(100000000)), 0, 10));
    test!("9", tok!(Lit(LitKind::Int(9)), 0, 0));
    test!("9876543210", tok!(Lit(LitKind::Int(9876543210)), 0, 9))
}

#[test]
fn line_comments() {
    test!("//hello this is a comment\r\n",);
    test!(
        "//hello this is a comment\r\n101",
        tok!(Lit(LitKind::Int(101)), 27, 29)
    )
}

#[test]
fn block_comments() {
    test!("/*block comment*/",);
    test!("/*block*comment*/",);
    test!(
        "hello/**/123",
        tok!(Ident("hello".to_string()), 0, 4),
        tok!(Lit(LitKind::Int(123)), 9, 11)
    );
    test!(
        "hello/***/123",
        tok!(Ident("hello".to_string()), 0, 4),
        tok!(Lit(LitKind::Int(123)), 10, 12)
    );
    test!(
        "hello/*****/123",
        tok!(Ident("hello".to_string()), 0, 4),
        tok!(Lit(LitKind::Int(123)), 12, 14)
    );
    test!("hello/*****123*/", tok!(Ident("hello".to_string()), 0, 4));
    test!(
        "/*block comment*/\n123 // Line comment\n /**/ 123",
        tok!(Lit(LitKind::Int(123)), 18, 20),
        tok!(Lit(LitKind::Int(123)), 44, 46)
    );
}

#[test]
fn empty_source() {
    test!("",)
}
