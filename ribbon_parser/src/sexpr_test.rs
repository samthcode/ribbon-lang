use super::*;

macro_rules! sexpr_test {
    ($src:literal, $($res:literal),+) => {
        {
            let program = Parser::new($src).parse();
            for err in program.diagnostics.iter() {
                eprintln!("{err}");
            }
            assert!(program.diagnostics.is_empty());
            assert_eq!(
                program
                    .body
                    .iter()
                    .map(|expr| expr.sexpr())
                    .collect::<Vec<String>>(),
                vec![$($res, )+]
            );
        }
    };
}

#[test]
fn binary_expressions() {
    sexpr_test!("1*2+3-4/5", "(- (+ (* 1 2) 3) (/ 4 5))")
}

#[test]
fn lists() {
    sexpr_test!(
        "[1*10,2,\"Hello World\"+10]",
        "(list (* 1 10) 2 (+ \"Hello World\" 10))"
    )
}

#[test]
fn unary_expressions() {
    sexpr_test!("-10", "(- 10)");
    sexpr_test!("*ident", "(* ident)");
    sexpr_test!("*ident.func", "(* (. ident func))");
    sexpr_test!("-ident", "(- ident)");
    sexpr_test!("!ident + 10", "(+ (! ident) 10)");
    sexpr_test!("&ident", "(& ident)");
}

#[test]
fn expression_or_tuple_or_parameter_list() {
    sexpr_test!("(1,2)", "(tuple 1 2)");
    sexpr_test!("(1,2,3,hello)", "(tuple 1 2 3 hello)");
    sexpr_test!("()", "(tuple)");
    sexpr_test!("(1)", "1");
    sexpr_test!("(hello+world)", "(+ hello world)");
}

#[test]
fn function_types() {
    sexpr_test!("() -> i32", "(fn-type (params) (ret (type (path i32))))");
    sexpr_test!(
        "(i32) -> String;",
        "(fn-type (params (type (path i32))) (ret (type (path String))))"
    )
}

#[test]
fn functions() {
    sexpr_test!("() => {}", "(fn (params) (ret ()) (body (block)))");
    sexpr_test!(
        "(a:i32) -> i32 {a+10}",
        "(fn (params (param (pattern a) (type (path i32)))) (ret (type (path i32))) (body (block\n    (+ a 10)\n)))"
    );
    sexpr_test!(
        "() => {a+10;;};",
        "(fn (params) (ret ()) (body (block\n    (+ a 10)\n)))"
    );
    sexpr_test!(
        "() -> i32 => \"Hello\".print",
        "(fn (params) (ret (type (path i32))) (body (. \"Hello\" print)))"
    )
}

#[test]
fn paths() {
    sexpr_test!("a::b::c", "a::b::c");
    sexpr_test!("hello::world", "hello::world")
}
