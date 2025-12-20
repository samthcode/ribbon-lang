use ribbon_ast::{BinOpKind, Expr, ExprKind, FunctionParameter, Path, Type};
use ribbon_error::{Diagnostic, ErrorKind};

pub trait TryNarrow {
    fn try_narrow_to_param<'a>(self) -> Result<Expr, Diagnostic<'a>>
    where
        Self: Sized;
    fn try_narrow_to_type<'a>(self) -> Result<Expr, Diagnostic<'a>>
    where
        Self: Sized;
}

impl TryNarrow for Expr {
    fn try_narrow_to_param<'a>(self) -> Result<Expr, Diagnostic<'a>>
    where
        Self: Sized,
    {
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
                    // TODO: Support full patterns
                    let ident = bin_op.lhs;
                    let ty = bin_op
                        .rhs
                        .try_narrow_to_type()
                        .map_err(|e| Diagnostic::new(e.kind, span))?;
                    Ok(Expr::new(
                        ExprKind::FunctionParameter(Box::new(FunctionParameter::new(
                            ident, ty, None,
                        ))),
                        span,
                    ))
                }
            },
            ExprKind::UnaryOp(_) => todo!(),
            ExprKind::Ident(_) => todo!(),
            ExprKind::Lit(_)
            | ExprKind::List(_)
            | ExprKind::TupleOrParameterList(_)
            | ExprKind::ParenthesisedExpression(_)
            | ExprKind::Tuple(_)
            | ExprKind::FunctionDeclaration(_)
            | ExprKind::FunctionTypeLike(_)
            | ExprKind::Block(_)
            | ExprKind::Path(_)
            | ExprKind::Type(_)
            | ExprKind::Binding(_)
            | ExprKind::Invalid => Err(Diagnostic::new_error(
                ErrorKind::ExpectedFunctionParameterFoundX(self.kind.description()),
                self.span,
            )),
        }
    }

    fn try_narrow_to_type<'a>(self) -> Result<Expr, Diagnostic<'a>>
    where
        Self: Sized,
    {
        match self.kind {
            ExprKind::Ident(_) => {
                let span = self.span;
                return Ok(Self::new(
                    ExprKind::Type(Type::new(Path::new(vec![self]), vec![])),
                    span,
                ));
            }
            ExprKind::Path(p) => {
                let span = self.span;
                return Ok(Self::new(ExprKind::Type(Type::new(p, vec![])), span));
            }
            ExprKind::Type(_) => return Ok(self),
            ExprKind::BinOp(_)
            | ExprKind::UnaryOp(_)
            | ExprKind::Lit(_)
            | ExprKind::List(_)
            | ExprKind::TupleOrParameterList(_)
            | ExprKind::ParenthesisedExpression(_)
            | ExprKind::Tuple(_)
            | ExprKind::FunctionDeclaration(_)
            | ExprKind::FunctionTypeLike(_)
            | ExprKind::FunctionParameter(_)
            | ExprKind::Block(_)
            | ExprKind::Binding(_)
            | ExprKind::Invalid => (),
        }

        let span = self.span;
        Err(Diagnostic::new_error(
            ErrorKind::ExpectedTypeFoundX(self.kind.description()),
            span,
        ))
    }
}
