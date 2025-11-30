use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Cmd,
}

#[derive(Subcommand)]
pub enum Cmd {
    /// run a Ribbon source file
    Run(Run),
}

#[derive(Args)]
pub struct Run {
    #[command(flatten)]
    pub source: Source,
    #[command(flatten)]
    pub source_opts: SourceOpts,
}

#[derive(Args)]
#[group(multiple = false)]
pub struct Source {
    /// source file to interpret
    pub file: Option<PathBuf>,
    /// expression to interpret
    #[arg(short, long)]
    pub expr: Option<String>,
}

#[derive(Args, Clone)]
pub struct SourceOpts {
    /// yield lexed tokens
    #[arg(short, long)]
    pub toks: bool,
    /// yield ast
    #[arg(short, long)]
    pub ast: bool,
}
