use ribbon_parser::Parser;

fn main() {
    let parser = Parser::new("[1+3,");
    let program = parser.parse();
    if !program.diagnostics.is_empty() {
        for e in program.diagnostics {
            eprintln!("{e}")
        }
    }
    for i in program.body {
        println!("{}", i.sexpr())
    }
}
