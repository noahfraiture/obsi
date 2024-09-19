use obsi::generator;
use obsi::lexer::Lexer;
use obsi::parser::Parser;

fn main() {
    let args = obsi::cli::args();
    let chars = obsi::read_file(&args.file).unwrap();
    let mut lexer = Lexer::new(chars); // Ideally this would move inside the crate to avoid have Lexer mod as public (same for Parser)
    match args.cmd {
        obsi::cli::Command::Lexer => {
            for token in lexer.by_ref() {
                println!("{:#?}", token);
            }
        }
        obsi::cli::Command::Parser => {
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            println!("{:#?}", program);
        }
        obsi::cli::Command::Compiler => {
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let result = generator::run(&program);
            println!("{}", result);
        }
    };
}
