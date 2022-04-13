/// The Interpreter for the Ribbon Programming Language
///
/// This handles lexing, parsing, and evaluating auto-magically!
pub struct Interpreter<'a> {
    // The source code
    source: &'a str,
    // The name of the file being run
    file_name: &'a str,
    // Show lexed tokens?
    tokens: bool,
    // Show parsed ast?
    ast: bool,
}

impl<'a> Interpreter<'a> {
    pub fn new(source: &'a str, file_name: &'a str) -> Self {
        Self {
            source,
            file_name,
            tokens: false,
            ast: false,
        }
    }

    pub fn with_options(source: &'a str, file_name: &'a str, tokens: bool, ast: bool) -> Self {
        Self {
            source,
            file_name,
            tokens,
            ast,
        }
    }

    pub fn interpret(&mut self) {
        let toks = match crate::lexer::Lexer::new(self.source).lex() {
            Ok(toks) => toks,
            Err(e) => {
                crate::error::eprint_error_message(e, self.file_name, self.source);
                return;
            }
        };
        if self.tokens {
            for i in &toks {
                println!("{}", i)
            }
        }

        let ast = match crate::parser::Parser::new(&*toks).parse() {
            Ok(ast) => ast,
            Err(e) => {
                crate::error::eprint_error_message(e, self.file_name, self.source);
                return;
            }
        };
        if self.ast {
            println!("{ast:#?}");
        }
    }
}
