//! This contains the necessary conctructs to represent the Abstract Syntax Tree of a Ribbon File

use indenter::indented;
use std::fmt::{self, Write};

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

impl fmt::Display for RootAstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            writeln!(f, "{node}")?;
        }
        Ok(())
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

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use AstNodeKind::*;
        // write!(f, "{}: ", self.span)?;
        match &self.kind {
            Literal(kind) => match kind {
                LiteralKind::String(s) => write!(f, "String \"{s}\""),
                LiteralKind::Integer(i) => write!(f, "Integer \"{i}\""),
                LiteralKind::Char(c) => write!(f, "Character \"{c}\""),
                LiteralKind::Bool(b) => write!(f, "Boolean \"{b}\""),
                LiteralKind::Float(tf) => write!(f, "Float \"{tf}\""),
            },
            Ident(ident) => write!(f, "Identifier \"{ident}\""),
            Call(name, args) => {
                write!(f, "Function Call \"{name}\"")?;
                if !args.is_empty() {
                    writeln!(f, " Args:")?;
                    for (ind, i) in args.iter().enumerate() {
                        write!(
                            indented(f),
                            "{i}{}",
                            if ind != args.len() - 1 { "\n" } else { "" }
                        )?;
                    }
                }
                Ok(())
            }
            CallOrPropertyAccess(name, arg) => write!(
                f,
                "Property Access or Function Call \"{name}\" Arg:\n    {arg}"
            ),

            VarDecl(name, value) => write!(f, "Variable Declaration \"{name}\" = {value}"),
        }
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
    VarDecl(Box<AstNode>, Box<AstNode>),
}
