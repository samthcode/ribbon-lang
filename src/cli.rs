use clap::{AppSettings, Parser, Subcommand};

/// A fictional versioning CLI
#[derive(Parser)]
#[clap(name = "ribbon")]
#[clap(about = "A programming language designed to be fast to write and heavily inspired by Rust.")]
#[clap(version)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs the Ribbon Interpreter on the given file
    #[clap(setting(AppSettings::ArgRequiredElseHelp))]
    Run {
        /// The .rbn file to run
        file: String,
        #[clap(long, short='t')]
        tokens: bool
    },
    /// Opens the REPL to test functions or get to grips with the Ribbon language!
    Repl
}

pub fn run() {
    let args = Cli::parse();

    match &args.command {
        Commands::Run {file, tokens} => {
            println!("So you want me to run this file: {}, With{} tokens shown?", file, if !tokens {" no"} else {""});
        },
        Commands::Repl => {
            println!("So you want me to open the REPL eh?");
        }
    }
}
