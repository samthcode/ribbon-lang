use serde::Serialize;

use ribbon_error::Diagnostic;

pub mod ast;
pub mod bin_op;
pub mod pattern;
pub mod unary_op;

pub use ast::*;
pub use bin_op::*;
pub use pattern::*;
pub use unary_op::*;

/// Structure containing a Ribbon module
///
/// This also contains all of the diagnostics associated with the module, if any exist
#[derive(Debug, Serialize, Default)]
pub struct Module<'a> {
    pub body: Vec<Expr>,
    pub diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> Module<'a> {
    pub fn push_expr(&mut self, expr: Expr) {
        self.body.push(expr);
    }

    pub fn push_diagnostic(&mut self, diag: Diagnostic<'a>) {
        self.diagnostics.push(diag);
    }
}

pub fn process_str(source: &str) -> String {
    let mut res = String::new();
    let mut chars = source.chars();
    // Skip the starting `"`
    chars.next();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                return res;
            }
            '\\' => match chars.next() {
                Some('\\') => res.push('\\'),
                Some('"') => res.push('"'),
                Some('n') => res.push('\n'),
                Some('r') => res.push('\r'),
                Some('t') => res.push('\t'),
                Some('0') => res.push('\0'),
                None => {
                    panic!("called `process_str` on invalid string literal")
                }
                _ => res.push(c),
            },
            _ => res.push(c),
        }
    }
    panic!("called `process_str` on invalid string literal")
}
