use ribbon_lexer::Lexer;
use ribbon_parser::Parser;

fn main() {
    let lexer = Lexer::new("const main := () -> { \"Hello World\" }");
    for i in lexer {
        println!("{}", i)
    }
    let parser = Parser::new("1+2;2*2/3");
    for i in parser.parse().0.body {
        println!("{}", i.sexpr())
    }
}
