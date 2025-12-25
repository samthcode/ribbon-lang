use ribbon_ast::{Expr, ExprKind::*, LitKind, process_str};
use ribbon_error::{Diagnostic, ErrorKind};
use ribbon_lexer::{KwKind, OpKind, Tok};

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
            ribbon_lexer::TokKind::Kw(kw_kind) => match kw_kind {
                KwKind::Const => todo!(),
                KwKind::Struct => todo!(),
                KwKind::Trait => todo!(),
                KwKind::Enum => parser.enumeration(),
                KwKind::Return => todo!(),
                KwKind::Use => todo!(),
                KwKind::For => todo!(),
                KwKind::While => todo!(),
            },
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
                OpKind::LCurly => Ok(parser.block()),
                // List expression
                OpKind::LSquare => Ok(parser.list()),
                // Tuple/unit type/function parameter list
                OpKind::LParen => Ok(parser.tuple_like()),
                // Unary operator
                op @ (OpKind::Minus | OpKind::Bang | OpKind::Mul | OpKind::Amp) => {
                    parser.unary_op(op, self.span)
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
        match self.kind {
            ribbon_lexer::TokKind::Op(op_kind) => match op_kind {
                // Standard binary operators
                OpKind::Plus
                | OpKind::Minus
                | OpKind::Mul
                | OpKind::Div
                | OpKind::Mod

                // Note that this can only be assignment NOT a binding, which will have already been handled
                | OpKind::Eq

                | OpKind::Tilde
                | OpKind::Amp
                | OpKind::Caret
                | OpKind::Pipe
                | OpKind::Lt
                | OpKind::Gt
                | OpKind::Dot
                | OpKind::EqEq
                | OpKind::BangEq
                | OpKind::LtEq
                | OpKind::GtEq
                | OpKind::AmpEq
                | OpKind::CaretEq
                | OpKind::PipeEq
                | OpKind::PlusEq
                | OpKind::MinusEq
                | OpKind::MulEq
                | OpKind::DivEq
                | OpKind::ModEq
                | OpKind::DotEq
                | OpKind::And
                | OpKind::Or
                | OpKind::DotDot
                | OpKind::ColonGt
                | OpKind::TildeGt
                | OpKind::TildeQuestion
                | OpKind::ShiftL
                | OpKind::ShiftR
                | OpKind::AndEq
                | OpKind::OrEq
                | OpKind::ShiftLEq
                | OpKind::ShiftREq
                | OpKind::DotDotEq => parser.binary_op(op_kind, self.span, lhs),
                // `path::to::something`
                OpKind::Path => parser.path(lhs),
                // Function type or function
                OpKind::MinusGt | OpKind::EqGt => parser.function(lhs),
                // Binding or method (which does not require parentheses)
                OpKind::Colon => parser.colon(lhs),

                OpKind::LParen => todo!(),
                OpKind::RParen => todo!(),
                OpKind::LSquare => todo!(),
                OpKind::RSquare => todo!(),
                OpKind::LCurly => todo!(),
                OpKind::RCurly => todo!(),
                OpKind::At => todo!(),

                // These should never occur as functions handling expressions using these handle them separately
                OpKind::Semi | OpKind::Comma => {
                    panic!("internal: unhandled semicolon or comma reached `try_left_denotation`")
                }

                kind @ (OpKind::Hash | OpKind::Bang | OpKind::Dollar) => Err(
                    Diagnostic::new_error(ErrorKind::UnexpectedOperator(kind), self.span),
                ),
            },
            ribbon_lexer::TokKind::Kw(_)
            | ribbon_lexer::TokKind::Lit(_)
            | ribbon_lexer::TokKind::Ident
            | ribbon_lexer::TokKind::Eof => Err(Diagnostic::new_error(
                ErrorKind::UnexpectedToken(self),
                self.span,
            )),
            ribbon_lexer::TokKind::Dummy => {
                panic!("internal: called `try_left_denotation` on dummy token")
            }
        }
    }
}
