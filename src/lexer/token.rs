use crate::{
    parser::{
        ast::{AstNode, AstNodeKind},
        Parser,
    },
    pos::Span,
};

pub static KEYWORDS: &[&str; 6] = &["mut", "if", "else", "while", "for", "type"];

pub static OPERATOR_CHARACTERS: &[char; 20] = &[
    '=', '+', '-', '<', '>', '*', '/', ':', ';', '.', '(', ')', '{', '}', '[', ']',
    /*Binding modifier*/ '$', '&', '|', '!',
];

macro_rules! ok_or_error_node {
    ($e: expr, $loc: expr) => {
        match $e {
            Ok(t) => t,
            Err(_) => return AstNode::new(AstNodeKind::ErrorFiller, $loc),
        }
    };
}

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
    pub binding_modified: bool,
    pub span: Span,
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

    pub fn nud(&self, p: &mut Parser) -> AstNode {
        match &self.kind {
            TokenKind::Literal(LiteralKind::String(str)) => AstNode::new(
                AstNodeKind::Literal(LiteralKind::String(str.clone())),
                self.span,
            ),
            TokenKind::Identifier(name) => {
                // TODO: Check for call
                AstNode::new(AstNodeKind::Ident(name.clone()), self.span)
            }
            TokenKind::OpenDelim(DelimKind::Parenthesis) => {
                let res = p.parse_bp(self.bp().1).unwrap();
                p.expect(TokenKind::ClosingDelim(DelimKind::Parenthesis));
                res
            }
            _ => todo!(),
        }
    }

    pub fn led(&self, p: &mut Parser, left: AstNode) -> AstNode {
        match &self.kind {
            TokenKind::Dot => {
                let name = ok_or_error_node!(
                    p.token_or_error(TokenKind::Identifier("".to_string()), self.span),
                    self.span
                );
                // TODO: Check for parens (so we can tell if it's a function)
                // TODO: If there are no parens, this needs to be AstNodeKind::CallOrPropertyAccess
                AstNode::new(
                    AstNodeKind::Call(Box::new(name.nud(p)), vec![left.clone()]),
                    Span::new(left.span.start, name.span.end),
                )
            }
            _ => todo!(),
        }
    }

    pub fn bp(&self) -> (u8, u8) {
        match &self.kind {
            TokenKind::Dot => (21, 20),
            TokenKind::OpenDelim(DelimKind::Parenthesis)
            | TokenKind::ClosingDelim(DelimKind::Parenthesis)
            | TokenKind::Newline
            | TokenKind::Semicolon => (0, 0),
            _ => todo!(),
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

impl TokenKind {
    #[allow(clippy::needless_return)]
    pub fn is_a(&self, other: &TokenKind) -> bool {
        match self {
            Self::Identifier(_) => return matches!(other, Self::Identifier(_)),
            Self::Literal(kind) => {
                if let Self::Literal(other_kind) = other {
                    return kind.is_a(other_kind);
                }
                return false;
            }
            _ => self == other,
        }
    }
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

impl LiteralKind {
    pub fn is_a(&self, other: &LiteralKind) -> bool {
        use LiteralKind::*;
        match self {
            Integer(_) => return matches!(other, Integer(_)),
            Float(_) => return matches!(other, Float(_)),
            String(_) => return matches!(other, String(_)),
            Char(_) => return matches!(other, Char(_)),
            Bool(_) => return matches!(other, Bool(_)),
        }
    }
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
