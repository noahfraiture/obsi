use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

mod cli;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn read_file(file_name: &str) -> Result<impl Iterator<Item = char>, io::Error> {
    let file = File::open(file_name)?;
    let reader = BufReader::new(file);
    let lines = reader.lines().collect::<Result<Vec<String>, io::Error>>()?;
    let chars = lines
        .into_iter()
        .flat_map(move |line| line.chars().take_while(|c| *c != '#').collect::<Vec<_>>());
    Ok(chars)
}

fn main() {
    let args = cli::args();
    let chars = read_file(&args.file).unwrap();
    let mut lexer = Lexer::new(chars);
    match args.cmd {
        cli::Command::Lexer => {
            while let Some(token) = lexer.next_token() {
                println!("{:#?}", token);
            }
        }
        cli::Command::Parser => {
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            println!("{:#?}", program);
        }
        cli::Command::Compiler => todo!(),
    };
}
