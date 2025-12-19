use serde::Serialize;

use ribbon_error::{Diagnostic, ErrorKind};
use ribbon_lexer::{Tok, TokKind, span::Span};

pub mod bin_op;
pub mod unary_op;

pub use bin_op::*;
pub use unary_op::*;

/// The root AST node for a Ribbon program
///
/// This contains a list of expressions which make up all of the functions, structs, enums, and
/// constants at the top level of a program, as well as all of the expressions contained within those expressions.
#[derive(Debug, Serialize)]
pub struct Program<'a> {
    pub body: Vec<Expr>,
    pub diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> Default for Program<'a> {
    fn default() -> Self {
        Self {
            body: vec![],
            diagnostics: vec![],
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }

    pub fn is(&self, other: ExprKind) -> bool {
        self.kind.is(other)
    }

    pub fn sexpr(&self) -> String {
        self.kind.sexpr()
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum ExprKind {
    BinOp(Box<BinOp>),
    UnaryOp(Box<UnaryOp>),
    Ident(Box<String>),
    Lit(LitKind),
    List(Vec<Expr>),
    TupleOrParameterList(Vec<Expr>),
    // This is only needed to correctly parse function declarations with a single parameter
    // (and no trailing comma inside the parameter list)
    ParenthesisedExpression(Box<Expr>),
    Tuple(Vec<Expr>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    /// A function type (or the start of a function definition) such as `(i32, i32) -> i32`
    /// This could also be `(a: i32, b: i32) -> i32` which is not a valid function type
    /// but could be a function declaration if followed by `=>`
    FunctionTypeLike(Box<FunctionTypeLike>),
    Block(Vec<Expr>),
    /// e.g. `a::b::c`
    Path(Path),
    /// Represents an invalid portion of code
    Invalid,
}

impl ExprKind {
    pub fn sexpr(&self) -> String {
        match self {
            ExprKind::BinOp(bin_op) => bin_op.sexpr(),
            ExprKind::UnaryOp(unary_op) => unary_op.sexpr(),
            ExprKind::Ident(s) => s.to_string(),
            ExprKind::Lit(lit_kind) => lit_kind.repr(),
            ExprKind::List(elems) => {
                format!(
                    "(list{}{})",
                    if elems.is_empty() { "" } else { " " },
                    space_sexprs(elems, " ")
                )
            }
            ExprKind::TupleOrParameterList(exprs) => {
                format!(
                    "(tuple-param-list{}{})",
                    if exprs.is_empty() { "" } else { " " },
                    space_sexprs(exprs, " ")
                )
            }
            ExprKind::ParenthesisedExpression(expr) => expr.sexpr(),
            ExprKind::Tuple(exprs) => {
                format!(
                    "(tuple{}{})",
                    if exprs.is_empty() { "" } else { " " },
                    space_sexprs(exprs, " ")
                )
            }
            ExprKind::FunctionDeclaration(fn_decl) => fn_decl.sexpr(),
            ExprKind::FunctionTypeLike(fn_ty_like) => fn_ty_like.sexpr(),
            ExprKind::Block(exprs) => {
                if exprs.len() == 0 {
                    return "(block)".to_string();
                }
                format!("(block\n    {}\n)", space_sexprs(exprs, "\n    "))
            }
            ExprKind::Path(path) => path.sexpr(),
            ExprKind::Invalid => "(<invalid>)".to_string(),
        }
    }

    pub fn is(&self, other: Self) -> bool {
        match self {
            ExprKind::BinOp { .. } => matches!(other, ExprKind::BinOp { .. }),
            ExprKind::UnaryOp { .. } => matches!(other, ExprKind::UnaryOp { .. }),
            ExprKind::Ident(_) => matches!(other, ExprKind::Ident(_)),
            ExprKind::Lit(_) => matches!(other, ExprKind::Lit(_)),
            ExprKind::List(_) => matches!(other, ExprKind::List(_)),
            ExprKind::TupleOrParameterList(_) => matches!(other, ExprKind::TupleOrParameterList(_)),
            ExprKind::ParenthesisedExpression(_) => {
                matches!(other, ExprKind::ParenthesisedExpression(_))
            }
            ExprKind::Tuple(_) => matches!(other, ExprKind::Tuple(_)),
            ExprKind::FunctionDeclaration(_) => {
                matches!(other, ExprKind::FunctionDeclaration(_))
            }
            ExprKind::FunctionTypeLike(_) => matches!(other, ExprKind::FunctionTypeLike(_)),
            ExprKind::Block(_) => matches!(other, ExprKind::Block(_)),
            ExprKind::Path(_) => matches!(other, ExprKind::Path(_)),
            ExprKind::Invalid => matches!(other, ExprKind::Invalid),
        }
    }
}

fn space_sexprs(exprs: &Vec<Expr>, sep: &str) -> String {
    exprs
        .iter()
        .map(|e| e.sexpr())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone, Serialize)]
pub struct Path {
    parts: Vec<Expr>,
}

impl Path {
    pub fn new(parts: Vec<Expr>) -> Self {
        Self { parts }
    }

    pub fn sexpr(&self) -> String {
        self.parts
            .iter()
            .map(|e| e.sexpr())
            .collect::<Vec<String>>()
            .join("::")
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDeclaration {
    parameters: Vec<Expr>,
    generic_parameters: Vec<Expr>,
    return_type: Option<Expr>,
    /// Can be either a single expression or a block
    body: Expr,
}

impl FunctionDeclaration {
    pub fn new(
        parameters: Vec<Expr>,
        generic_parameters: Vec<Expr>,
        return_type: Option<Expr>,
        body: Expr,
    ) -> Self {
        Self {
            parameters,
            generic_parameters,
            return_type,
            body,
        }
    }

    pub fn sexpr(&self) -> String {
        format!(
            "(fn {}(params{}) (ret {}) (body {}))",
            if self.generic_parameters.len() > 0 {
                format!(
                    "(generics {}) ",
                    space_sexprs(&self.generic_parameters, " ")
                )
            } else {
                "".to_string()
            },
            format!(
                "{}{}",
                if self.parameters.len() > 0 { " " } else { "" },
                space_sexprs(&self.parameters, " ")
            ),
            match &self.return_type {
                Some(ty) => ty.sexpr(),
                None => "()".to_string(),
            },
            self.body.sexpr()
        )
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionTypeLike {
    pub parameters: Vec<Expr>,
    pub generic_parameters: Vec<Expr>,
    pub return_type: Expr,
}

impl FunctionTypeLike {
    pub fn new(parameters: Vec<Expr>, generic_parameters: Vec<Expr>, return_type: Expr) -> Self {
        Self {
            parameters,
            generic_parameters,
            return_type,
        }
    }

    pub fn sexpr(&self) -> String {
        format!(
            "(fn-type {}(params{}) (ret {}))",
            if self.generic_parameters.len() > 0 {
                format!(
                    "(generics {}) ",
                    space_sexprs(&self.generic_parameters, " ")
                )
            } else {
                "".to_string()
            },
            format!(
                "{}{}",
                if self.parameters.len() > 0 { " " } else { "" },
                space_sexprs(&self.parameters, " ")
            ),
            self.return_type.sexpr()
        )
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum LitKind {
    /// e.g. `42`
    Int(i64),
    /// e.g. `"Hello World!"`
    Str(Box<String>),
    /// e.g. `42.0`
    Float(f64),
    /// true or false
    Bool(bool),
}

impl LitKind {
    pub fn repr(&self) -> String {
        match self {
            LitKind::Int(i) => i.to_string(),
            LitKind::Str(s) => format!("\"{s}\""),
            LitKind::Float(f) => f.to_string(),
            LitKind::Bool(b) => b.to_string(),
        }
    }
}

pub fn tok_to_expr(tok: Tok) -> Result<Expr, Diagnostic> {
    use ExprKind::*;
    use ribbon_lexer::LitKind as LLK;
    Ok(Expr::new(
        match tok.kind {
            TokKind::Ident => Ident(Box::new(tok.source.to_string())),
            TokKind::Lit(kind) => match kind {
                // TODO: At some point, we need to find the true size of the integer
                LLK::Int => Lit(LitKind::Int(
                    tok.source
                        .replace("_", "")
                        .parse()
                        .expect("internal: failed to parse int literal"),
                )),
                LLK::UnprocessedStr => Lit(LitKind::Str(Box::new(process_str(tok.source)))),
                LLK::InvalidStr(invalid_str_kind) => {
                    return Err(Diagnostic::new_error(
                        ErrorKind::InvalidStringLiteral(invalid_str_kind),
                        tok.span,
                    ));
                }
                LLK::Float => Lit(LitKind::Float(
                    tok.source
                        .replace("_", "")
                        .parse()
                        .expect("internal: failed to parse float literal"),
                )),
                LLK::Bool => Lit(LitKind::Bool(
                    tok.source
                        .parse()
                        .expect("interal: failed to parse boolean literal"),
                )),
            },
            _ => {
                return Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedToken(tok),
                    tok.span,
                ));
            }
        },
        tok.span,
    ))
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
