use clap::Parser as _;

mod cli;
use cli::Cmd;

use ribbon_lexer::Lexer;
use ribbon_parser::Parser;

fn main() {
    let args = cli::Cli::parse();

    match &args.command {
        Cmd::Run(run) => {
            let source = if let Some(path) = &run.source.file {
                std::fs::read_to_string(path).unwrap()
            } else {
                run.source.expr.clone().unwrap()
            };

            let toks = Lexer::new(source.as_str()).into_iter();
            if run.source_opts.toks {
                for t in toks.clone() {
                    println!("[{}-{}] {t}", t.span.low, t.span.hi)
                }
            }

            let program = Parser::from_tok_stream(source.as_str(), toks).parse();
            if run.source_opts.ast {
                for i in program.body.iter() {
                    println!("{}", i.sexpr())
                }
            }
            for e in program.diagnostics {
                eprintln!("{e}")
            }
        }
    };
}
