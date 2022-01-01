use crate::pos::Span;

/// The Token created by the Lexer and used by the Parser to generate an AST
pub struct Token<'a> {
    token: TokenKind<'a>,
    span: Span,
}

impl<'a> Token<'a> {
    pub fn new(token: TokenKind<'a>, span: Span) -> Self {
        Self { token, span }
    }
}

pub enum TokenKind<'a> {
    /// A newline needs to be tokenised to delimit the end of an expression
    Newline,

    /// i.e. abcd, hello_there, TestingTesting123
    Identifier(&'a str),
    /// i.e. while, for
    Keyword(KeywordKind),

    /// String, Number, Character, Boolean
    Literal(LiteralKind<'a>),

    /// :
    Colon,
    /// .
    Dot,
    /// =
    Assignemnt,

    /// i.e. (, {, [
    OpenDelim(DelimKind),
    /// i.e. ), }, ]
    ClosingDelim(DelimKind),

    /// i.e. +, -, *, /, ** (exponent)
    BinOp(BinOpKind),
    /// i.e +=, -=, /=, *=
    BindOpEq(BinOpKind),

    /// i.e. <, >, <=, >=, ==
    Equality(EqualityKind),
}

pub enum LiteralKind<'a> {
    Numeric(i64),
    String(&'a str),
    Char(char),
    Bool(bool),
}

pub enum DelimKind {
    Parenthesis,
    SquareBracket,
    CurlyBracket,
}

pub enum KeywordKind {
    Function,
    While,
    For,
    Struct,
    Enum,
    If,
    Else,
}

pub enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Exp,
}

pub enum EqualityKind {
    LT,
    GT,
    LTE,
    GTE,
    EQ
}