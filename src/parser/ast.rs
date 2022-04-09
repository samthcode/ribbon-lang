use crate::lexer::token::LiteralKind;

/// The abstract representation of Ribbon code before it is evaluated
///
/// This enum holds every possible Ribbon construct. It is created by the parser to be interperted by the evaluator.
///
/// # Examples
///
/// ```rust
/// vec![AstNodeKind::Call(
///    Box::new(AstNodeKind::Ident("print")),
///    vec![AstNodeKind::Literal(LiteralKind::String(String::from(
///        "Hello World",
///    )))],
/// )];
/// ```
pub enum AstNodeKind<'a> {
    /// A literal, such as a string or character
    Literal(LiteralKind),
    /// Identifier, such as 'foo', 'bar', 'baz
    Ident(&'a str),
    /// Call to a function. The 'callee' will be the first parameter if the function is called on an object.
    Call(Box<AstNodeKind<'a>>, Vec<AstNodeKind<'a>>),
    PropertyAccessor(Box<AstNodeKind<'a>>, Box<AstNodeKind<'a>>),
    VarDecl(Box<AstNodeKind<'a>>, Box<AstNodeKind<'a>>),
}
