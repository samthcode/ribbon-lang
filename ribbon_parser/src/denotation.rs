use ribbon_ast::{Expr, ExprKind::*, LitKind, process_str};
use ribbon_error::{Diagnostic, ErrorKind};
use ribbon_lexer::{OpKind, Tok};

use crate::Parser;

pub trait TryDenotation<'a> {
    fn try_null_denotation(self, parser: &mut Parser<'a>) -> Result<Expr, Diagnostic<'a>>;

    fn try_left_denotation(
        self,
        parser: &mut Parser<'a>,
        lhs: Expr,
    ) -> Result<Expr, Diagnostic<'a>>;
}

impl<'a> TryDenotation<'a> for Tok<'a> {
    fn try_null_denotation(self, parser: &mut Parser<'a>) -> Result<Expr, Diagnostic<'a>> {
        use ribbon_lexer::LitKind as Llk;
        match self.kind {
            ribbon_lexer::TokKind::Ident => Ok(Expr::new(
                Ident(Box::new(self.source.to_string())),
                self.span,
            )),
            ribbon_lexer::TokKind::Kw(kw_kind) => todo!(),
            ribbon_lexer::TokKind::Lit(lit_kind) => Ok(Expr::new(
                match lit_kind {
                    // TODO: At some point, we need to find the true size of the integer
                    Llk::Int => Lit(LitKind::Int(
                        self.source
                            .replace("_", "")
                            .parse()
                            .expect("internal: failed to parse int literal"),
                    )),
                    Llk::UnprocessedStr => Lit(LitKind::Str(Box::new(process_str(self.source)))),
                    Llk::InvalidStr(invalid_str_kind) => {
                        return Err(Diagnostic::new_error(
                            ErrorKind::InvalidStringLiteral(invalid_str_kind),
                            self.span,
                        ));
                    }
                    Llk::Float => Lit(LitKind::Float(
                        self.source
                            .replace("_", "")
                            .parse()
                            .expect("internal: failed to parse float literal"),
                    )),
                    Llk::Bool => Lit(LitKind::Bool(
                        self.source
                            .parse()
                            .expect("interal: failed to parse boolean literal"),
                    )),
                },
                self.span,
            )),
            ribbon_lexer::TokKind::Op(op_kind) => match op_kind {
                // Block expression
                OpKind::LCurly => Ok(parser.block_expr(self.span)),
                // List expression
                OpKind::LSquare => Ok(parser.list_expr(self.span)),
                // Tuple/unit type/function parameter list
                OpKind::LParen => Ok(parser.tuple_like_expr(self.span)),
                // Unary operator
                op @ (OpKind::Minus | OpKind::Bang | OpKind::Mul | OpKind::Amp) => {
                    parser.unary_expr(op, self.span)
                }
                // Unexpected (closing) delimiter
                op if op.is_delim() => Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedDelimiter(op),
                    self.span,
                )),
                k => Err(Diagnostic::new_error(
                    ErrorKind::UnexpectedOperator(k),
                    self.span,
                )),
            },
            ribbon_lexer::TokKind::Eof => Err(Diagnostic::new_error(
                ErrorKind::UnexpectedToken(self),
                self.span,
            )),
            ribbon_lexer::TokKind::Dummy => {
                panic!("internal: called `try_null_denotation` on dummy token")
            }
        }
    }

    fn try_left_denotation(
        self,
        parser: &mut Parser<'a>,
        lhs: Expr,
    ) -> Result<Expr, Diagnostic<'a>> {
        todo!()
    }
}
