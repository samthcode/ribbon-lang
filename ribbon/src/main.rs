use ribbon_lexer::Lexer;
use ribbon_parser::Parser;

fn main() {
    let lexer = Lexer::new("const main := () -> { \"Hello World\" }");
    for i in lexer {
        println!("{}", i)
    }
    let parser = Parser::new("[1+3,");
    let program = parser.parse();
    if !program.1.is_empty() {
        for e in program.1 {
            eprintln!("{e}")
        }
    }
    for i in program.0.body {
        println!("{}", i.sexpr())
    }
}
