#![allow(dead_code)]

pub(crate) mod ast;

use ast::{BinOp, Expr, Literal, PreOp, Precedence, Program, Size, Stmt};

use crate::lexer::{Lexer, Token};
use core::panic;
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

    /// Consume the next token if it matches the expected token
    fn check_next(&mut self, token: Token) -> bool {
        if self.lexer.peek() == Some(&token) {
            self.lexer.next();
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Program {
        // TODO: This is not terrible. Instead your lexer should implement iterator so this can just be a map operation
        let mut statements = vec![];
        while self.lexer.peek().is_some() {
            statements.push(self.parse_stmt());
        }
        ast::Program(statements)
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.lexer.peek().unwrap() {
            // Try to avoid unwrap. It creates panic without any context
            Token::Ident(_) => self.parse_stmt_assign(),
            Token::Int(_) => self.parse_stmt_declare(),
            Token::At => self.parse_stmt_func(),
            Token::Tilde => self.parse_stmt_return(),
            Token::Question => self.parse_stmt_if(),
            Token::Dollar => self.parse_stmt_expr(),
            Token::Ptr => self.parse_stmt_declare(),
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
            | Token::LBrace
            | Token::RBrace
            | Token::Xor
            | Token::Or
            | Token::And
            | Token::Comma
            | Token::Backtick
            | Token::Less => {
                panic!("Unexpected token {:?}", self.lexer.peek())
            }
        }
    }

    fn parse_stmt_assign(&mut self) -> Stmt {
        let ident = self.lexer.next();
        if let Some(Token::Ident(name)) = ident {
            let expr = self.parse_expr(Precedence::Lowest);
            Stmt::Assignment(name, expr)
        } else {
            panic!("Expected identifier, got {:?}", ident);
        }
    }

    fn parse_stmt_declare(&mut self) -> Stmt {
        let size = match self.lexer.next().take() {
            Some(Token::Int(size)) => Size::Int(size as u32),
            Some(Token::Ptr) => Size::Ptr,
            token => panic!("Expected function size, got {:?}", token),
        };

        let name = match self.lexer.next().take() {
            Some(Token::Ident(name)) => name,
            token => panic!("Expected function name, got {:?}", token),
        };
        Stmt::Declare(size, name)
    }

    // NOTE: I search to iterate two by two on the lexer but didn't find a way that work
    fn parse_stmt_func(&mut self) -> Stmt {
        self.lexer.next(); // Ignore '@'

        let size = match self.lexer.next().take() {
            Some(Token::Int(size)) => size as u32,
            token => panic!("Expected function size, got {:?}", token),
        };

        let name = match self.lexer.next().take() {
            Some(Token::Ident(name)) => name,
            token => panic!("Expected function name, got {:?}", token),
        };

        // Parse function arguments
        let mut args = Vec::new();
        while !self.check_next(Token::LBrace) {
            let arg_size = match self.lexer.next().take() {
                Some(Token::Int(arg_size)) => arg_size as u32,
                token => panic!("Expected argument size, got {:?}", token),
            };

            let arg_name = match self.lexer.next().take() {
                Some(Token::Ident(arg_name)) => arg_name,
                token => panic!("Expected argument name, got {:?}", token),
            };

            args.push((arg_size, arg_name));
        }

        // TODO : add return check
        Stmt::Function(size, name, args, Box::new(self.parse_stmt_block()))
    }

    fn parse_stmt_block(&mut self) -> Stmt {
        let mut statements = Vec::new();
        while self.lexer.peek().is_some() {
            if self.check_next(Token::RBrace) {
                break;
            }
            statements.push(self.parse_stmt());
        }
        Stmt::BlockStatement(statements)
    }

    fn parse_stmt_if(&mut self) -> Stmt {
        self.lexer.next(); // consume '?'
        let condition = self.parse_expr(Precedence::Lowest);

        if !self.check_next(Token::LBrace) {
            panic!("Expected '{{', got {:?}", self.lexer.peek());
        }

        let then_stmt = self.parse_stmt_block();

        let else_stmt = if self.check_next(Token::LBrace) {
            Some(self.parse_stmt_block())
        } else {
            None
        };

        Stmt::If(condition, Box::new(then_stmt), else_stmt.map(Box::new))
    }

    fn parse_stmt_return(&mut self) -> Stmt {
        self.lexer.next();
        Stmt::Return(self.parse_expr(Precedence::Lowest))
    }

    fn parse_stmt_expr(&mut self) -> Stmt {
        let expr = self.parse_expr(Precedence::Lowest);
        Stmt::Expression(expr)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Expr {
        let mut left = self.parse_expr_prefix();
        while precedence < self.lexer.peek().unwrap().into() {
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
        match self.lexer.next().take().unwrap() {
            Token::Ident(ident) => Expr::Variable(ident),
            Token::Int(value) => Expr::Literal(Literal::Int(value)),
            Token::Float(value) => Expr::Literal(Literal::Float(value)),
            Token::LParenth => self.parse_expr(Precedence::Lowest),
            Token::Bang => {
                Expr::Prefix(PreOp::Not, Box::new(self.parse_expr((&Token::Bang).into())))
            }
            Token::Comma => Expr::Prefix(
                PreOp::Deref,
                Box::new(self.parse_expr((&Token::Bang).into())),
            ),
            Token::Backtick => {
                Expr::Prefix(PreOp::Ref, Box::new(self.parse_expr((&Token::Bang).into())))
            }
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
            Token::Dollar => todo!(),
            Token::Xor => todo!(),
            Token::Or => todo!(),
            Token::And => todo!(),
            Token::Ptr => todo!(),
        }
    }

    fn parse_expr_group(&mut self) -> Expr {
        let expr = self.parse_expr(Precedence::Lowest);
        match self.lexer.next().take().unwrap() {
            Token::RParenth => expr,
            token => panic!("Expected ')', got {:?}", token),
        }
    }

    // The boolean indicates if the infix expression is finished or not
    fn parse_expr_infix(&mut self, left: Expr) -> (Expr, bool) {
        match self.lexer.next().as_ref().unwrap() {
            Token::Plus => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Add,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Dash => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Sub,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Slash => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Div,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Modulo => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Mod,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Asterix => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Mul,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Equal => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Equal,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Less => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Less,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Xor=> (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Xor,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Or => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::Or,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::And => (
                Expr::Infix(
                    Box::new(left),
                    BinOp::And,
                    Box::new(self.parse_expr(Precedence::Sum)),
                ),
                true,
            ),
            Token::Dollar => (self.parse_expr_call(left), true),
            Token::LParenth // TODO
            | Token::RParenth
            | Token::Ident(_)
            | Token::Int(_)
            | Token::At
            | Token::Bang
            | Token::Comma
            | Token::Backtick
            | Token::Float(_)
            | Token::Question
            | Token::Tilde
            | Token::LBrace
            | Token::Ptr
            | Token::RBrace => (left, false),
        }
    }

    // NOTE : should the call be only usable with function name, thus String instead of expr
    // Or allow pointer reference for function call
    fn parse_expr_call(&mut self, left: Expr) -> Expr {
        Expr::Call(Box::new(left), self.parse_expr_args())
    }

    fn parse_expr_args(&mut self) -> Vec<Expr> {
        todo!()
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
