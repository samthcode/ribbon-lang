use crate::pos::Span;

pub static KEYWORDS: &[&str; 6] = &["mut", "if", "else", "while", "for", "type"];

pub static OPERATOR_CHARACTERS: &[char; 20] = &[
    '=', '+', '-', '<', '>', '*', '/', ':', ';', '.', '(', ')', '{', '}', '[', ']',
    /*Binding modifier*/ '$', '&', '|', '!',
];

/// The Token created by the Lexer and used by the Parser to generate an AST
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
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
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            binding_modified: false,
            span,
        }
    }

    pub fn with_binding(kind: TokenKind, binding_modified: bool, span: Span) -> Self {
        Self {
            kind,
            binding_modified,
            span,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {:?}{}",
            self.span,
            self.kind,
            if self.binding_modified {
                " (Binding modified)"
            } else {
                ""
            }
        )
    }
}

// TODO: Add the other tokens in
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    /// A newline needs to be tokenised to delimit the end of an expression
    Newline,

    /// i.e. abcd, hello_there, TestingTesting123
    Identifier(String),
    /// i.e. while, for
    Keyword(String),

    /// String, Number, Character, Boolean
    Literal(LiteralKind),

    /// :
    Colon,
    /// ::
    ScopeResolutionOperator,
    /// .
    Dot,
    /// =
    Assignment,
    /// ;
    Semicolon,
    /// |
    Pipe,
    /// =>
    FatArrow,
    /// ?
    QuestionMark,
    /// ++,
    Increment,
    /// --,
    Decrement,

    /// i.e. (, {, [
    OpenDelim(DelimKind),
    /// i.e. ), }, ]
    ClosingDelim(DelimKind),

    /// i.e. +, -, *, /, ** (exponent)
    ArithmeticOp(ArithmeticOpKind),
    /// i.e +=, -=, /=, *=, **=
    ArithmeticOpEq(ArithmeticOpKind),

    /// i.e. <, >, <=, >=, ==
    ComparativeOp(ComparativeOpKind),

    // && etc
    LogicalOp(LogicalOpKind),
}

impl TryFrom<String> for TokenKind {
    type Error = ();
    fn try_from(str: String) -> Result<Self, Self::Error> {
        match &*str {
            // Misc operators
            "::" => Ok(Self::ScopeResolutionOperator),
            "?" => Ok(Self::QuestionMark),
            ":" => Ok(Self::Colon),
            "." => Ok(Self::Dot),
            ";" => Ok(Self::Semicolon),
            "=" => Ok(Self::Assignment),
            "|" => Ok(Self::Pipe),
            "=>" => Ok(Self::FatArrow),
            "++" => Ok(Self::Increment),
            "--" => Ok(Self::Decrement),
            // Delimeters
            "(" => Ok(Self::OpenDelim(DelimKind::Parenthesis)),
            ")" => Ok(Self::ClosingDelim(DelimKind::Parenthesis)),
            "[" => Ok(Self::OpenDelim(DelimKind::SquareBracket)),
            "]" => Ok(Self::ClosingDelim(DelimKind::SquareBracket)),
            "{" => Ok(Self::OpenDelim(DelimKind::CurlyBracket)),
            "}" => Ok(Self::ClosingDelim(DelimKind::CurlyBracket)),
            // Equalities
            "==" => Ok(Self::ComparativeOp(ComparativeOpKind::EQ)),
            "<" => Ok(Self::ComparativeOp(ComparativeOpKind::LT)),
            ">" => Ok(Self::ComparativeOp(ComparativeOpKind::GT)),
            "<=" => Ok(Self::ComparativeOp(ComparativeOpKind::LTE)),
            ">=" => Ok(Self::ComparativeOp(ComparativeOpKind::GTE)),
            // Arithmetic Operators
            "+" => Ok(Self::ArithmeticOp(ArithmeticOpKind::Add)),
            "-" => Ok(Self::ArithmeticOp(ArithmeticOpKind::Sub)),
            "*" => Ok(Self::ArithmeticOp(ArithmeticOpKind::Mul)),
            "/" => Ok(Self::ArithmeticOp(ArithmeticOpKind::Div)),
            "**" => Ok(Self::ArithmeticOp(ArithmeticOpKind::Exp)),
            // Arithmetic-Equals Operators
            "+=" => Ok(Self::ArithmeticOpEq(ArithmeticOpKind::Add)),
            "-=" => Ok(Self::ArithmeticOpEq(ArithmeticOpKind::Sub)),
            "*=" => Ok(Self::ArithmeticOpEq(ArithmeticOpKind::Mul)),
            "/=" => Ok(Self::ArithmeticOpEq(ArithmeticOpKind::Div)),
            "**=" => Ok(Self::ArithmeticOpEq(ArithmeticOpKind::Exp)),
            // Logical Operators
            "&&" => Ok(Self::LogicalOp(LogicalOpKind::And)),
            "||" => Ok(Self::LogicalOp(LogicalOpKind::Or)),
            "!" => Ok(Self::LogicalOp(LogicalOpKind::Not)),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for TokenKind {
    type Error = ();
    fn try_from(str: &str) -> Result<Self, Self::Error> {
        TokenKind::try_from(str.to_string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
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
    Ifp,
    Whilep,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ComparativeOpKind {
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NEQ,
}

#[derive(Clone, Debug, PartialEq)]

pub enum LogicalOpKind {
    And,
    Or,
    Not,
}
