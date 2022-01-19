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
        let toks = crate::lexer::Lexer::new(self.source)
            .lex()
            .unwrap_or_else(|errs| {
                crate::error::eprint_error_message(errs, self.file_name, self.source.to_string());
                std::process::exit(1);
            });
        if self.tokens {
            for i in toks {
                println!("{}", i)
            }
        }
    }
}
