use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader};

pub mod cli;
pub mod generator;
pub mod lexer;
pub mod parser;

pub fn read_file(file_name: &str) -> Result<impl Iterator<Item = char>, io::Error> {
    let file = File::open(file_name)?;
    let reader = BufReader::new(file);
    let lines = reader.lines().collect::<Result<Vec<String>, io::Error>>()?;
    let chars = lines
        .into_iter()
        .flat_map(move |line| line.chars().take_while(|c| *c != '#').collect::<Vec<_>>());
    Ok(chars)
}
