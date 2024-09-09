use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

mod lexer;
mod parser;

use lexer::Lexer;

fn read_file(file_name: &str) -> Result<impl Iterator<Item = char>, io::Error> {
    let file = File::open(file_name)?;
    let reader = BufReader::new(file);
    let lines = reader.lines().collect::<Result<Vec<String>, io::Error>>()?;
    let ignore_chars = [' ', '\t', '\n', '\r'];
    let chars = lines.into_iter().flat_map(move |line| {
        line.chars()
            .filter(|c| !ignore_chars.contains(c))
            .collect::<Vec<_>>()
    });
    Ok(chars)
}

fn main() {
    let chars = read_file("test.txt").unwrap();
    let mut lexer = Lexer::new(chars);
    while let Some(token) = lexer.next_token() {
        println!("{:?}", token);
    }
}
