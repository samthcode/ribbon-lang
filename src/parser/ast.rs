//! This contains the necessary conctructs to represent the Abstract Syntax Tree of a Ribbon File

use crate::{lexer::token::LiteralKind, pos::Span};

#[derive(Default, Debug)]
/// The root node of a program.
///
/// This contains a list of nodes which represent each expression/statement of a Ribbon program.
pub struct RootAstNode {
    nodes: Vec<AstNode>,
}

impl RootAstNode {
    pub fn new(nodes: Vec<AstNode>) -> Self {
        Self { nodes }
    }

    pub fn push(&mut self, node: AstNode) {
        self.nodes.push(node)
    }
}

#[derive(Debug, Clone, PartialEq)]
/// A node containing a Ribbon expression/statement
pub struct AstNode {
    pub kind: AstNodeKind,
    pub span: Span,
}

impl AstNode {
    pub fn new(kind: AstNodeKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// One part of the abstract representation of Ribbon code before it is evaluated.
///
/// This enum holds every possible Ribbon construct. This is used in [AstNode](`AstNode`) along with a [Span](`ribbon::pos::Span`) to represent the AST
#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeKind {
    /// A literal, such as a string or character
    Literal(LiteralKind),
    /// Identifier, such as 'foo', 'bar', 'baz'
    Ident(String),
    /// Call to a function. The syntactic sugar `object.function(args?)` will use this too
    Call(Box<AstNode>, Vec<AstNode>),
    /// Call to a function as the `object.function` syntactic sugar without parenthesis OR object accessor
    CallOrPropertyAccess(Box<AstNode>, Box<AstNode>),
    PropertyAccessor(Box<AstNode>, Box<AstNode>),
    VarDecl(Box<AstNode>, Box<AstNode>),
}
