#[allow(unused)]
use insta::assert_yaml_snapshot;

use crate::Parser;

macro_rules! create_tests {
    ($($name:ident: $test_string:literal),+$(,)?) => {
        $(
            #[test]
            fn $name() {
                let program = Parser::new($test_string).parse();
                insta::assert_yaml_snapshot!(&program)
            }
        )+
    };
}

create_tests! {
    line_comment: "// Hello World",
    block_comment: "/* Hello there\nHow do you do*/",
    addition: "1+2",
    subtraction: "3-10",
    multiplication: "3*3",
    division: "10/2",
    basic_precedence: "10+2/2-3*4",
    list: "[1,2,\"test\",ident]",
    unclosed_list: "[1,2,3",
    unary_minus: "-10",
    deref: "*ident",
    unary_not: "!true",
    r#ref: "&ident",
    tuple: "(1,2)", // TODO: Or param list at the moment
    unclosed_tuple: "(1,2",
    fn_decl_start_or_unit_type: "()",
    parenthesised_expression: "(10)"
}
