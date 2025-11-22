use ribbon_lexer::Lexer;
use ribbon_parser::Parser;

fn main() {
    let lexer = Lexer::new("const main := () -> { \"Hello World\" }");
    for i in lexer {
        println!("{}", i)
    }
    let parser = Parser::new("hello + world");
    println!("{:#?}", parser.parse().0)
}
