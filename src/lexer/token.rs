use crate::pos::Span;

/// The Token created by the Lexer and used by the Parser to generate an AST
#[derive(Debug, PartialEq)]
pub struct Token {
    token: TokenKind,
    /// This field is necessary to denote the possible addition of the Binding Modifier Operator.
    /// This is the `$` operator and is used to boost the precedence / binding power of the token.
    ///
    /// This allows for expressions such as `10 > 5 ? 10 : 20 +$ 30` to evealuate to 40 (whereas without the binding modifier, it would have evaluated to 10).
    ///
    /// This cannot be applied / won't have an effect on certain tokens such as literals, newlines, identifiers or keywords.
    binding_modified: bool,
    span: Span,
}

impl Token {
    pub fn new(token: TokenKind, span: Span) -> Self {
        Self {
            token,
            binding_modified: false,
            span,
        }
    }

    pub fn with_binding(token: TokenKind, binding_modified: bool, span: Span) -> Self {
        Self {
            token,
            binding_modified,
            span,
        }
    }
}

// TODO: Add the other tokens in
#[derive(Debug, PartialEq)]
pub enum TokenKind {
    /// A newline needs to be tokenised to delimit the end of an expression
    Newline,

    /// i.e. abcd, hello_there, TestingTesting123
    Identifier(String),
    /// i.e. while, for
    Keyword(KeywordKind),

    /// String, Number, Character, Boolean
    Literal(LiteralKind),

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

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub enum DelimKind {
    Parenthesis,
    SquareBracket,
    CurlyBracket,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordKind {
    Function,
    While,
    For,
    Struct,
    Enum,
    If,
    Else,
}

#[derive(Debug, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Exp,
}

#[derive(Debug, PartialEq)]
pub enum EqualityKind {
    LT,
    GT,
    LTE,
    GTE,
    EQ,
}
