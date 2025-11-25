use ribbon_parser::Parser;

fn main() {
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
