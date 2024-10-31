use crate::{
    error::Error,
    parser::{
        ast::{AstNode, AstNodeKind},
        Parser,
    },
    pos::Span,
};

pub static KEYWORDS: &[&str; 6] = &["mut", "if", "else", "while", "for", "type"];

pub static OPERATOR_CHARACTERS: &[char; 21] = &[
    '=', '+', '-', '<', '>', '*', '/', ':', ';', '.', '(', ')', '{', '}', '[', ']',
    /*Binding modifier*/ '$', '&', '|', '!', ',',
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

    pub fn nud(&self, p: &mut Parser) -> Result<AstNode, Error> {
        match &self.kind {
            TokenKind::Literal(LiteralKind::String(str)) => Ok(AstNode::new(
                AstNodeKind::Literal(LiteralKind::String(str.clone())),
                self.span,
            )),
            TokenKind::Identifier(name) => {
                Ok(AstNode::new(AstNodeKind::Ident(name.clone()), self.span))
            }
            TokenKind::LParen => {
                let res = p.parse_bp_allowing_newlines(self.kind.nud_bp().1, true)?;
                let _ = p.expect(TokenKind::RParen);
                Ok(res)
            }
            TokenKind::LBracket => {
                let (vals, pos) = p.parse_list(vec![], TokenKind::RBracket)?;
                Ok(AstNode::new(
                    AstNodeKind::List(vals),
                    Span::new(self.span.start, pos),
                ))
            }
            _ => todo!(),
        }
    }

    pub fn led(&self, p: &mut Parser, left: AstNode) -> Result<AstNode, Error> {
        match &self.kind {
            TokenKind::Dot => {
                let name = p.expect(TokenKind::Identifier("".to_string()))?.nud(p)?;

                if let Some(Token {
                    kind: TokenKind::LParen,
                    ..
                }) = p.peek()
                {
                    p.advance();

                    let (args, end) = p.parse_list(vec![left], TokenKind::RParen)?;

                    return Ok(AstNode::new(
                        AstNodeKind::Call(Box::new(name.clone()), args),
                        Span::new(name.span.start, end),
                    ));
                }

                Ok(AstNode::new(
                    AstNodeKind::CallOrPropertyAccess(
                        Box::new(name.clone()),
                        Box::new(left.clone()),
                    ),
                    Span::new(left.span.start, name.span.end),
                ))
            }
            TokenKind::LParen => {
                let (args, end) = p.parse_list(vec![], TokenKind::RParen)?;

                Ok(AstNode::new(
                    AstNodeKind::Call(Box::new(left.clone()), args),
                    Span::new(left.span.start, end),
                ))
            }
            TokenKind::ArithmeticOp(k) => {
                let right = p.parse_bp(self.kind.led_bp().0)?;
                Ok(AstNode::new(
                    AstNodeKind::BinOp(k.clone(), Box::new(left.clone()), Box::new(right.clone())),
                    Span::new(left.span.start, right.span.end),
                ))
            }
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
    /// A comment is tokenised for any future use in documentation
    Comment(String),

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
    // ;
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
    /// ,
    Comma,

    /// (
    LParen,
    /// [
    LBracket,
    /// {
    LCurly,
    /// )
    RParen,
    /// ]
    RBracket,
    /// }
    RCurly,

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

    pub fn nud_bp(&self) -> (u8, u8) {
        match &self {
            TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::LBracket
            | TokenKind::RBracket
            | TokenKind::Newline
            | TokenKind::Semicolon => (0, 0),
            // Call args separator
            TokenKind::Comma => (6, 5),
            TokenKind::Dot => (21, 20),
            k => {
                println!("{k:?}");
                todo!()
            }
        }
    }

    pub fn led_bp(&self) -> (u8, u8) {
        match &self {
            TokenKind::ArithmeticOp(kind) => match kind {
                ArithmeticOpKind::Add | ArithmeticOpKind::Sub => (11, 10),
                ArithmeticOpKind::Div | ArithmeticOpKind::Mul | ArithmeticOpKind::Exp => (13, 12),
            },
            _ => self.nud_bp(),
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
            "," => Ok(Self::Comma),
            // Delimeters
            "(" => Ok(Self::LParen),
            ")" => Ok(Self::RParen),
            "[" => Ok(Self::LBracket),
            "]" => Ok(Self::RBracket),
            "{" => Ok(Self::LCurly),
            "}" => Ok(Self::RCurly),
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
            Integer(_) => matches!(other, Integer(_)),
            Float(_) => matches!(other, Float(_)),
            String(_) => matches!(other, String(_)),
            Char(_) => matches!(other, Char(_)),
            Bool(_) => matches!(other, Bool(_)),
        }
    }
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

impl std::fmt::Display for ArithmeticOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                ArithmeticOpKind::Add => "+",
                ArithmeticOpKind::Sub => "-",
                ArithmeticOpKind::Div => "/",
                ArithmeticOpKind::Mul => "*",
                ArithmeticOpKind::Exp => "**",
            }
        )
    }
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
