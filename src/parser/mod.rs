#![allow(dead_code)]

mod ast;

use ast::{BinOp, Expr, Literal, Precedence, Program, Stmt};

use crate::lexer::{Lexer, Token};
use core::panic;

pub struct Parser<'a> {
    current: Option<Token>,
    next: Option<Token>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> { // Could also implement Iterator to be able to use standard iterator functions and maps
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        Parser {
            current: None,
            next: lexer.next_token(),
            lexer,
        }
    }

    /// Get the next token and consume it
    fn next_token(&mut self) -> &mut Option<Token> {
        self.current = self.next.take();
        self.next = self.lexer.next_token();
        &mut self.current
    }

    /// Get the next token without consuming it
    fn peak_token(&mut self) -> &mut Option<Token> {
        &mut self.next
    }

    /// Consume the next token if it matches the expected token
    fn check_next(&mut self, token: Token) -> bool {
        if self.peak_token().as_ref() == Some(&token) {
            self.next_token();
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Program {
        // TODO: This is not terrible. Instead your lexer should implement iterator so this can just be a map operation
        let mut statements = vec![];
        while self.lexer.has_next() {
            statements.push(self.parse_stmt());
        }
        ast::Program(statements)
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peak_token().as_ref().unwrap() { // Try to avoid unwrap. It creates panic without any context
            Token::Ident(_) => self.parse_stmt_assign(),
            Token::Int(_) => self.parse_stmt_declare(),
            Token::At => self.parse_stmt_func(),
            Token::Tilde => self.parse_stmt_return(),
            Token::Question => self.parse_stmt_if(),
            Token::Bang
            | Token::Float(_)
            | Token::LParenth
            | Token::RParenth
            | Token::Plus
            | Token::Dash
            | Token::Slash
            | Token::Modulo
            | Token::Asterix
            | Token::Equal
            | Token::Less => {
                panic!("Unexpected token {:?}", self.peak_token().as_ref().unwrap())
            }
            Token::LBrace => todo!(), // TODO : block statement
            Token::RBrace => todo!(),
        }
    }

    fn parse_stmt_assign(&mut self) -> Stmt {
        let ident = self.next_token().take().unwrap();
        if let Token::Ident(name) = ident {
            let expr = self.parse_expr(Precedence::Lowest);
            Stmt::Assignment(name, expr)
        } else {
            panic!("Expected identifier, got {:?}", ident);
        }
    }

    fn parse_stmt_declare(&mut self) -> Stmt {
        match (
            self.next_token().take().unwrap(),
            self.next_token().take().unwrap(),
        ) {
            (Token::Int(size), Token::Ident(name)) => Stmt::Declare(size as i8, name),
            (Token::Int(_), unexpected) => panic!("Expected identifier, got {:?}", unexpected),
            (unexpected, _) => panic!("Expected size, got {:?}", unexpected),
        }
    }

    fn parse_stmt_func(&mut self) -> Stmt {
        self.next_token(); // we ignore '@'
        match (
            self.next_token().take().unwrap(),
            self.next_token().take().unwrap(),
        ) {
            (Token::Int(size), Token::Ident(name)) => {
                // parse function arguments
                let mut args = Vec::new();
                while !self.check_next(Token::LBrace) {
                    match (
                        self.next_token().take().unwrap(),
                        self.next_token().take().unwrap(),
                    ) {
                        (Token::Int(arg_size), Token::Ident(arg_name)) => {
                            args.push((arg_size as i8, arg_name))
                        }
                        (size, name) => panic!("Expected size and name, got {name:?} and {size:?}"),
                    }
                }

                Stmt::Function(size as i8, name, args, Box::new(self.parse_stmt_block()))
            }
            // TODO: Okish to compile during compilation but should have a dedicated system for this
            (size, name) => panic!("Excepted size and name, got {name:?} and {size:?}"),
        }
    }

    fn parse_stmt_block(&mut self) -> Stmt {
        let mut statements = Vec::new();
        while self.peak_token().is_some() {
            if self.check_next(Token::RBrace) {
                break;
            }
            statements.push(self.parse_stmt());
        }
        Stmt::BlockStatement(statements)
    }

    fn parse_stmt_if(&mut self) -> Stmt {
        self.next_token(); // consume '?'
        let condition = self.parse_expr(Precedence::Lowest);

        if !self.check_next(Token::LBrace) {
            panic!("Expected '{{', got {:?}", self.peak_token());
        }

        let consequence = self.parse_stmt_block();

        let alternative = if self.check_next(Token::LBrace) {
            Some(self.parse_stmt_block())
        } else {
            None
        };

        Stmt::If(condition, Box::new(consequence), alternative.map(Box::new))
    }

    fn parse_stmt_return(&mut self) -> Stmt {
        self.next_token();
        Stmt::Return(self.parse_expr(Precedence::Lowest))
    }

    fn parse_stmt_expr(&mut self) -> Stmt {
        let expr = self.parse_expr(Precedence::Lowest);
        Stmt::Expression(expr)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Expr {
        let mut left = self.parse_expr_prefix();
        while precedence < self.peak_token().as_ref().unwrap().into() {
            match self.parse_expr_infix(left) {
                (infix, true) => left = infix,
                (infix, false) => {
                    left = infix;
                    break;
                }
            }
        }
        left
    }

    fn parse_expr_prefix(&mut self) -> Expr {
        match self.next_token().take().unwrap() {
            Token::Ident(ident) => Expr::Variable(ident),
            Token::Int(value) => Expr::Literal(Literal::Int(value)),
            Token::Bang => Expr::Not(Box::new(
                self.parse_expr((&Token::Bang).into()),
            )),
            Token::Float(value) => Expr::Literal(Literal::Float(value)),
            Token::LParenth => self.parse_expr(Precedence::Lowest),
            Token::RParenth => todo!(),
            Token::At => todo!(),
            Token::Plus => todo!(),
            Token::Dash => todo!(),
            Token::Slash => todo!(),
            Token::Modulo => todo!(),
            Token::Asterix => todo!(),
            Token::Equal => todo!(),
            Token::Less => todo!(),
            Token::Question => todo!(),
            Token::Tilde => todo!(),
            Token::LBrace => todo!(),
            Token::RBrace => todo!(),
        }
    }

    fn parse_expr_group(&mut self) -> Expr {
        let expr = self.parse_expr(Precedence::Lowest);
        match self.next_token().take().unwrap() {
            Token::RParenth => expr,
            token => panic!("Expected ')', got {:?}", token),
        }
    }

    fn parse_expr_infix(&mut self, left: Expr) -> (Expr, bool) {
        match self.next_token().as_ref().unwrap() {
            Token::Plus => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Add,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Dash => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Sub,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Slash => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Div,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Modulo => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Mod,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Asterix => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Mul,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Equal => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Equal,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Less => (
                Expr::BinaryOp(
                    Box::new(left),
                    BinOp::Less,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::LParenth => (self.parse_expr_call(left), true),
            Token::RParenth
            | Token::Ident(_)
            | Token::Int(_)
            | Token::At
            | Token::Bang
            | Token::Float(_)
            | Token::Question
            | Token::Tilde
            | Token::LBrace
            | Token::RBrace => (left, false),
        }
    }

    fn parse_expr_call(&mut self, left: Expr) -> Expr {
        Expr::Call(Box::new(left), self.parse_expr_args())
    }

    fn parse_expr_args(&mut self) -> Vec<Expr> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    ~ d
}"#;

        // Expected parsed program representation
        let expected_program = Program(vec![Stmt::Function(
            4,
            "compute".to_string(),
            vec![(4, "a".to_string()), (4, "b".to_string())],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(4, "five".to_string()),
                Stmt::Declare(4, "c".to_string()),
                Stmt::Declare(4, "d".to_string()),
                Stmt::Assignment("five".to_string(), Expr::Literal(Literal::Int(5))),
                Stmt::Assignment(
                    "c".to_string(),
                    Expr::BinaryOp(
                        Box::new(Expr::Variable("a".to_string())),
                        BinOp::Add,
                        Box::new(Expr::Variable("b".to_string())),
                    ),
                ),
                Stmt::If(
                    Expr::BinaryOp(
                        Box::new(Expr::Variable("b".to_string())),
                        BinOp::Less,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ),
                    Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Add,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )])),
                    Some(Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Sub,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )]))),
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
