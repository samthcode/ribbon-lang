use crate::Parser;

macro_rules! create_tests {
    ($($name:ident: $test_string:literal),+$(,)?) => {
        $(
            #[test]
            fn $name() {
                let program = Parser::new($test_string).parse();
                insta::with_settings!({
                    description => $test_string,
                    omit_expression => true
                }, {
                    insta::assert_yaml_snapshot!(&program)
                })
            }
        )+
    };
}

create_tests! {
    empty_source: "",
    line_comment: "// Hello World",
    block_comment: "/* Hello there\nHow do you do*/",
    addition: "1+2",
    subtraction: "3-10",
    multiplication: "3*3",
    division: "10/2",
    basic_precedence: "10+2/2-3*4",
    logical_operators: "a && b || !d",
    list: "[1,2,\"test\",ident]",
    unclosed_list: "[1,2,3",
    unary_minus: "-10",
    deref: "*ident",
    unary_not: "!true",
    r#ref: "&ident",
    tuple: "(1,2)", // TODO: Or param list at the moment
    unclosed_tuple: "(1,2",
    fn_decl_start_or_unit_type: "()",
    parenthesised_expression: "(10)",
    plain_function: "() => {}",
    function_with_return_type: "() -> i32 {}",
    function_with_parameters: "(a: i32, b: i32) => a + b",
    function_with_parameters_explicit_ret_type:
        "(a: i32, b: i32) -> i32 => a + b;",
    invalid_function_parameter: "(a:\"test\") => a",
    path: "a::b::c",
    path_2: "hello::world",
    invalid_path: "hello::",
}
