use ribbon_lexer::span::Span;
use serde::Serialize;

use crate::{BinOp, Pattern, UnaryOp};

#[derive(Debug, Clone, Serialize, Default)]
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

#[derive(Debug, Clone, Serialize, Default)]
pub enum ExprKind {
    /// A pattern-matching assignment through `:=`
    Binding(Box<Binding>),
    BinOp(Box<BinOp>),
    UnaryOp(Box<UnaryOp>),
    Ident(Box<String>),
    Lit(LitKind),
    List(Vec<Expr>),
    // This is only needed to correctly parse function declarations with a single parameter
    // (and no trailing comma inside the parameter list)
    ParenthesisedExpression(Box<Expr>),
    Tuple(Vec<Expr>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    /// A function type (or the start of a function definition) such as `(i32, i32) -> i32`
    /// This could also be `(a: i32, b: i32) -> i32` which is not a valid function type
    /// but could be a function declaration if followed by `=>`
    FunctionType(Box<FunctionType>),
    FunctionParameter(Box<FunctionParameter>),
    Block(Vec<Expr>),
    /// e.g. `a::b::c`
    Path(Path),
    /// e.g. `i32, String Vec[i32]`
    Type(Type),
    /// Any valid pattern
    Pattern(Pattern),
    /// Represents an invalid portion of code
    #[default]
    Invalid,
}

impl ExprKind {
    pub fn sexpr(&self) -> String {
        match self {
            ExprKind::Binding(binding) => binding.sexpr(),
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
            ExprKind::ParenthesisedExpression(expr) => expr.sexpr(),
            ExprKind::Tuple(exprs) => {
                format!(
                    "(tuple{}{})",
                    if exprs.is_empty() { "" } else { " " },
                    space_sexprs(exprs, " ")
                )
            }
            ExprKind::FunctionDeclaration(fn_decl) => fn_decl.sexpr(),
            ExprKind::FunctionType(fn_ty_like) => fn_ty_like.sexpr(),
            ExprKind::FunctionParameter(fn_param) => fn_param.sexpr(),
            ExprKind::Block(exprs) => {
                if exprs.is_empty() {
                    return "(block)".to_string();
                }
                format!("(block\n    {}\n)", space_sexprs(exprs, "\n    "))
            }
            ExprKind::Path(path) => path.sexpr(),
            ExprKind::Type(ty) => ty.sexpr(),
            ExprKind::Pattern(pat) => pat.sexpr(),
            ExprKind::Invalid => "(<invalid>)".to_string(),
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            ExprKind::Binding(_) => "binding",
            ExprKind::BinOp(_) => "binary operator",
            ExprKind::UnaryOp(_) => "unary operator",
            ExprKind::Ident(_) => "identifier",
            ExprKind::Lit(lit_kind) => lit_kind.description(),
            ExprKind::List(_) => "list",
            ExprKind::ParenthesisedExpression(_) => "parenthesised expression",
            ExprKind::Tuple(_) => "tuple",
            ExprKind::FunctionDeclaration(_) => "function declaration",
            ExprKind::FunctionType(_) => "function type or beginning of function declaration",
            ExprKind::FunctionParameter(_) => "function parameter",
            ExprKind::Block(_) => "block",
            ExprKind::Path(_) => "path",
            ExprKind::Type(_) => "type",
            ExprKind::Pattern(_) => "pattern",
            ExprKind::Invalid => "<invalid>",
        }
    }

    pub fn is(&self, other: Self) -> bool {
        match self {
            ExprKind::Binding(_) => matches!(other, ExprKind::Binding(_)),
            ExprKind::BinOp { .. } => matches!(other, ExprKind::BinOp { .. }),
            ExprKind::UnaryOp { .. } => matches!(other, ExprKind::UnaryOp { .. }),
            ExprKind::Ident(_) => matches!(other, ExprKind::Ident(_)),
            ExprKind::Lit(_) => matches!(other, ExprKind::Lit(_)),
            ExprKind::List(_) => matches!(other, ExprKind::List(_)),
            ExprKind::ParenthesisedExpression(_) => {
                matches!(other, ExprKind::ParenthesisedExpression(_))
            }
            ExprKind::Tuple(_) => matches!(other, ExprKind::Tuple(_)),
            ExprKind::FunctionDeclaration(_) => {
                matches!(other, ExprKind::FunctionDeclaration(_))
            }
            ExprKind::FunctionType(_) => matches!(other, ExprKind::FunctionType(_)),
            ExprKind::FunctionParameter(_) => matches!(other, ExprKind::FunctionParameter(_)),
            ExprKind::Block(_) => matches!(other, ExprKind::Block(_)),
            ExprKind::Path(_) => matches!(other, ExprKind::Path(_)),
            ExprKind::Type(_) => matches!(other, ExprKind::Type(_)),
            ExprKind::Pattern(_) => matches!(other, ExprKind::Pattern(_)),
            ExprKind::Invalid => matches!(other, ExprKind::Invalid),
        }
    }
}

fn space_sexprs(exprs: &[Expr], sep: &str) -> String {
    exprs
        .iter()
        .map(|e| e.sexpr())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct Binding {
    pub pat: Expr,
    pub ty: Option<Expr>,
    pub rhs: Expr,
}

impl Binding {
    pub fn new(pat: Expr, ty: Option<Expr>, rhs: Expr) -> Self {
        Self { pat, ty, rhs }
    }

    pub fn sexpr(&self) -> String {
        format!(
            "(binding (pat {}){} (rhs {}))",
            self.pat.sexpr(),
            if let Some(ty) = &self.ty {
                format!(" {}", ty.sexpr())
            } else {
                "".to_string()
            },
            self.rhs.sexpr()
        )
    }
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct FunctionParameter {
    pat: Expr,
    ty: Expr,
    default_value: Option<Expr>,
}

impl FunctionParameter {
    pub fn new(pat: Expr, ty: Expr, default_value: Option<Expr>) -> Self {
        Self {
            pat,
            ty,
            default_value,
        }
    }

    pub fn sexpr(&self) -> String {
        format!(
            "(param {} {}{})",
            self.pat.sexpr(),
            self.ty.sexpr(),
            if let Some(v) = &self.default_value {
                format!(" (default-value {})", v.sexpr())
            } else {
                "".to_string()
            }
        )
    }
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct Type {
    path: Path,
    generic_parameters: Vec<Expr>,
}

impl Type {
    pub fn new(path: Path, generic_parameters: Vec<Expr>) -> Self {
        Self {
            path,
            generic_parameters,
        }
    }

    pub fn sexpr(&self) -> String {
        format!(
            "(type (path {}){})",
            self.path.sexpr(),
            if self.generic_parameters.is_empty() {
                "".to_string()
            } else {
                format!(
                    "(generics {}) ",
                    space_sexprs(&self.generic_parameters, " ")
                )
            }
        )
    }
}

#[derive(Debug, Clone, Serialize, Default)]
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

#[derive(Debug, Clone, Serialize, Default)]
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

    #[allow(clippy::format_in_format_args)]
    pub fn sexpr(&self) -> String {
        format!(
            "(fn {}(params{}) (ret {}) (body {}))",
            if !self.generic_parameters.is_empty() {
                format!(
                    "(generics {}) ",
                    space_sexprs(&self.generic_parameters, " ")
                )
            } else {
                "".to_string()
            },
            format!(
                "{}{}",
                if !self.parameters.is_empty() { " " } else { "" },
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

#[derive(Debug, Clone, Serialize, Default)]
pub struct FunctionType {
    pub parameters: Vec<Expr>,
    pub generic_parameters: Vec<Expr>,
    pub return_type: Expr,
}

impl FunctionType {
    pub fn new(parameters: Vec<Expr>, generic_parameters: Vec<Expr>, return_type: Expr) -> Self {
        Self {
            parameters,
            generic_parameters,
            return_type,
        }
    }

    #[allow(clippy::format_in_format_args)]
    pub fn sexpr(&self) -> String {
        format!(
            "(fn-type {}(params{}) (ret {}))",
            if !self.generic_parameters.is_empty() {
                format!(
                    "(generics {}) ",
                    space_sexprs(&self.generic_parameters, " ")
                )
            } else {
                "".to_string()
            },
            format!(
                "{}{}",
                if !self.parameters.is_empty() { " " } else { "" },
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

    pub fn description(&self) -> &'static str {
        match self {
            LitKind::Int(_) => "integer literal",
            LitKind::Str(_) => "string literal",
            LitKind::Float(_) => "float literal",
            LitKind::Bool(_) => "boolean literal",
        }
    }
}

impl Default for LitKind {
    fn default() -> Self {
        Self::Int(0)
    }
}
