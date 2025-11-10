use ribbon_lexer::Lexer;

fn main() {
    let lexer = Lexer::new("hello");
    for i in lexer {
        println!("{:?}", i)
    }
}
