use clap::{Parser, Subcommand};

/// Compiler
#[derive(Parser, Debug)]
#[command(name = "Obsi")]
#[command(author = "Noah")]
#[command(version)]
#[command(help_template = "
{name} - {about}

Author: {author}
Version: {version}

{usage-heading} {usage}
{all-args} {tab}")]
pub struct Cli {
    /// Action to perform with the data
    #[command(subcommand)]
    pub cmd: Command,

    /// File to read
    #[arg(short, long)]
    pub file: String,
}

#[derive(Subcommand, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Command {
    /// Print the token from the file
    Lexer,
    /// Print the AST from the file
    Parser,
    /// Produce a binary from the file
    Compiler,
}

pub fn args() -> Cli {
    Cli::parse()
}
