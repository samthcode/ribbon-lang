use std::{error::Error, iter::Peekable};

use ribbon_ast as ast;
use ribbon_lexer as lexer;

use ast::Program;
use lexer::{Lexer, TokStream};

mod prec;

/// The parser for the Ribbon programming language.
///
/// This takes a source string (temporary) and first lexes it, and then turns
/// the resulting stream of tokens into an AST with a root Prorgam node.
pub struct Parser<'a> {
    source: &'a str,
    tok_stream: Peekable<TokStream<'a>>,
    program: Program,
    errors: Vec<Box<dyn Error>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            tok_stream: Lexer::new(source).into_iter().peekable(),
            program: Default::default(),
            errors: vec![],
        }
    }

    /// The method that turns the tok_stream into a root Program node
    ///
    /// This will need its own error struct/trait & result type
    pub fn parse(self) -> (Program, Vec<Box<dyn Error>>) {
        // TODO: Parse everything
        // should be simple...
        (self.program, self.errors)
    }
}
