pub(crate) mod ast;
mod parse;

use ast::{Program, Stmt};

use crate::lexer::{Lexer, Token};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    // Could also implement Iterator to be able to use standard iterator functions and maps
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Program {
        // NOTE : idk how to use a map here to have a cleaner function
        let mut statements = vec![];
        while self.lexer.peek().is_some() {
            statements.push(self.parse_stmt());
        }
        Program(statements)
    }

    /// Consume the next token if it matches the expected token
    fn check_next(&mut self, token: Token) -> bool {
        if self.lexer.peek() == Some(&token) {
            self.lexer.next();
            true
        } else {
            false
        }
    }
}

pub fn has_return(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Declare(_, _) => false,
        Stmt::Assignment(_, _) => false,
        Stmt::Expression(_) => false,
        Stmt::Function(_, _, _, _) => unreachable!(),
        Stmt::Return(_) => true,
        Stmt::BlockStatement(block) => block.iter().all(has_return),
        Stmt::If(_, c, a) => has_return(c) && a.as_ref().map_or(true, |a| has_return(a)),
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::Lexer;
    use super::ast::*;
    use super::Parser;

    #[test]
    fn test_compute() {
        let input = r#"
@4 compute 4 a 4 b {
    4 five 
    4 c
    4 d
    five 5 
    c a + b 
    ? b < 5 {
        d a + 5
    } {
        d a - 5
    }
    P ptr
    4 deref
    ptr `five
    deref ,ptr
    ~ d
}"#;

        // Expected parsed program representation
        let expected_program = Program(vec![Stmt::Function(
            4,
            "compute".to_string(),
            vec![(4, "a".to_string()), (4, "b".to_string())],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(4), "five".to_string()),
                Stmt::Declare(Size::Int(4), "c".to_string()),
                Stmt::Declare(Size::Int(4), "d".to_string()),
                Stmt::Assignment("five".to_string(), Expr::Literal(Literal::Int(5))),
                Stmt::Assignment(
                    "c".to_string(),
                    Expr::Infix(
                        Box::new(Expr::Variable("a".to_string())),
                        BinOp::Add,
                        Box::new(Expr::Variable("b".to_string())),
                    ),
                ),
                Stmt::If(
                    Expr::Infix(
                        Box::new(Expr::Variable("b".to_string())),
                        BinOp::Less,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ),
                    Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::Infix(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Add,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )])),
                    Some(Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::Infix(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Sub,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )]))),
                ),
                Stmt::Declare(Size::Ptr, "ptr".to_string()),
                Stmt::Declare(Size::Int(4), "deref".to_string()),
                Stmt::Assignment(
                    "ptr".to_string(),
                    Expr::Prefix(PreOp::Ref, Box::new(Expr::Variable("five".to_string()))),
                ),
                Stmt::Assignment(
                    "deref".to_string(),
                    Expr::Prefix(PreOp::Deref, Box::new(Expr::Variable("ptr".to_string()))),
                ),
                Stmt::Return(Expr::Variable("d".to_string())),
            ])),
        )]);

        // Use lexer and parser to parse the input string into an actual program
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        let parsed_program = parser.parse();

        // Assert that the parsed program matches the expected program representation
        assert_eq!(parsed_program, expected_program);
    }
}
