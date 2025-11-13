use ribbon_lexer::Lexer;

fn main() {
    let lexer = Lexer::new("const main := () -> { \"Hello World\" }");
    for i in lexer {
        println!("{}", i)
    }
}
