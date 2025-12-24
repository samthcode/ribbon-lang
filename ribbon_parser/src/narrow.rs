use ribbon_ast::{BinOpKind, Expr, ExprKind, FunctionParameter, Path, Pattern, PatternKind, Type};
use ribbon_error::{Diagnostic, ErrorKind};

pub trait TryNarrow: Sized {
    fn try_narrow_to_param<'a>(self) -> Result<Expr, Diagnostic<'a>>;
    fn try_narrow_to_type<'a>(self) -> Result<Expr, Diagnostic<'a>>;
    fn try_narrow_to_pattern<'a>(self) -> Result<Expr, Diagnostic<'a>>;
}

impl TryNarrow for Expr {
    fn try_narrow_to_param<'a>(self) -> Result<Expr, Diagnostic<'a>> {
        let span = self.span;

        use BinOpKind::*;
        match self.kind {
            ExprKind::FunctionParameter(_) => Ok(self),

            // Parameter with default value
            ExprKind::Binding(binding) if binding.ty.is_some() => Ok(Expr::new(
                ExprKind::FunctionParameter(Box::new(FunctionParameter::new(
                    binding.pat,
                    binding.ty.unwrap(),
                    Some(binding.rhs),
                ))),
                self.span,
            )),
            ExprKind::BinOp(bin_op) => match bin_op.kind {
                // All variants given in the case that new ones are addded in the future
                // TODO(maybe): case of default values for parameters
                Add | Sub | Mul | Div | Mod | Lt | Gt | Equality | Inequality | LtEquality
                | GtEquality | BwAnd | BwXor | BwOr | Assign | BwAndAssign | BwXorAssign
                | BwOrAssign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
                | DotAssign | LogicalAnd | LogicalOr | MethodPipe | NonMethodPipe | ErrProp
                | ShiftL | ShiftR | LogicalAndAssign | LogicalOrAssign | ShiftLAssign
                | ShiftRAssign | ThinArrow | FatArrow | MethodParen | NonMethod | Path
                | RangeExclusive | RangeInclusive => Err(Diagnostic::new_error(
                    ErrorKind::ExpectedFunctionParameterFoundX(
                        // Expr::new(ExprKind::BinOp(bin_op), span).kind.description(),
                        // It's very unlikely this will change
                        "binary operator",
                    ),
                    span,
                )),
                // `a:B`, which is a parameter in some contexts (i.e. function declarations)
                // `a` could be any pattern such a destructuring e.g. `[c,d]`
                // `B` is a type - an Ident or a list accessor (generic argument(s))
                MethodNoParen => {
                    let pat = bin_op
                        .lhs
                        .try_narrow_to_pattern()
                        .map_err(|e| Diagnostic::new(e.kind, span))?;
                    let ty = bin_op
                        .rhs
                        .try_narrow_to_type()
                        .map_err(|e| Diagnostic::new(e.kind, span))?;
                    Ok(Expr::new(
                        ExprKind::FunctionParameter(Box::new(FunctionParameter::new(
                            pat, ty, None,
                        ))),
                        span,
                    ))
                }
            },
            ExprKind::UnaryOp(_) => todo!(),
            ExprKind::Ident(_) => todo!(),
            ExprKind::Lit(_)
            | ExprKind::List(_)
            | ExprKind::ParenthesisedExpression(_)
            | ExprKind::Tuple(_)
            | ExprKind::FunctionDeclaration(_)
            | ExprKind::FunctionType(_)
            | ExprKind::Block(_)
            | ExprKind::Path(_)
            | ExprKind::Type(_)
            | ExprKind::Binding(_)
            | ExprKind::Pattern(_)
            | ExprKind::Enum(_)
            | ExprKind::Invalid => Err(Diagnostic::new_error(
                ErrorKind::ExpectedFunctionParameterFoundX(self.kind.description()),
                self.span,
            )),
        }
    }

    fn try_narrow_to_type<'a>(self) -> Result<Expr, Diagnostic<'a>> {
        let span = self.span;
        match self.kind {
            ExprKind::Type(_) => Ok(self),

            ExprKind::Ident(_) => Ok(Self::new(
                ExprKind::Type(Type::new(Path::new(vec![self]), vec![])),
                span,
            )),
            ExprKind::Path(p) => Ok(Self::new(ExprKind::Type(Type::new(p, vec![])), span)),

            ExprKind::BinOp(_)
            | ExprKind::UnaryOp(_)
            | ExprKind::Lit(_)
            | ExprKind::List(_)
            | ExprKind::ParenthesisedExpression(_)
            | ExprKind::Tuple(_)
            | ExprKind::FunctionDeclaration(_)
            | ExprKind::FunctionType(_)
            | ExprKind::FunctionParameter(_)
            | ExprKind::Block(_)
            | ExprKind::Binding(_)
            | ExprKind::Pattern(_)
            | ExprKind::Enum(_)
            | ExprKind::Invalid => Err(Diagnostic::new_error(
                ErrorKind::ExpectedTypeFoundX(self.kind.description()),
                span,
            )),
        }
    }

    fn try_narrow_to_pattern<'a>(self) -> Result<Expr, Diagnostic<'a>> {
        match self.kind {
            ExprKind::Pattern(_) => Ok(self),

            ExprKind::Ident(ident) => Ok(Expr::new(
                ExprKind::Pattern(Pattern::new(PatternKind::Identifier(*ident))),
                self.span,
            )),
            ExprKind::List(_) => todo!(),
            ExprKind::ParenthesisedExpression(_) => todo!(),
            ExprKind::Tuple(_) => todo!(),
            // TODO: Structs, wildcards, etc.

            // `|`, `..`, `..=`, etc.
            ExprKind::BinOp(_) => todo!(),

            ExprKind::Binding(_)
            | ExprKind::Block(_)
            | ExprKind::Path(_)
            | ExprKind::Invalid
            | ExprKind::UnaryOp(_)
            | ExprKind::Lit(_)
            | ExprKind::FunctionDeclaration(_)
            | ExprKind::FunctionType(_)
            | ExprKind::FunctionParameter(_)
            | ExprKind::Enum(_)
            | ExprKind::Type(_) => Err(Diagnostic::new_error(
                ErrorKind::ExpectedPatternFoundX(self.kind.description()),
                self.span,
            )),
        }
    }
}
