use crate::lexer::token::LiteralKind;

enum AstNodeKind<'a> {
    /// A literal, such as a string or character
    Literal(LiteralKind),
    /// Identifier, such as 'foo', 'bar', 'baz
    Ident(&'a str),
    /// Call to a function. The 'callee' will be the first parameter if the function is called on an object.
    Call { args: Vec<AstNodeKind<'a>> },
}
