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

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        Parser {
            current: None,
            next: lexer.next_token(),
            lexer,
        }
    }

    fn next_token(&mut self) -> &mut Option<Token> {
        self.current = self.next.take();
        self.next = self.lexer.next_token();
        &mut self.current
    }

    fn peak_token(&mut self) -> &mut Option<Token> {
        &mut self.next
    }

    pub fn parse(&mut self) -> Program {
        let mut statements = vec![];
        while self.lexer.has_next() {
            statements.push(self.parse_stmt());
        }
        ast::Program(statements)
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peak_token().as_ref().unwrap() {
            Token::Ident(_) => self.parse_stmt_assign(),
            Token::Int(_) => self.parse_stmt_declare(),
            Token::At => self.parse_stmt_func(),
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
            | Token::Less
            | Token::Colon
            | Token::Question
            | Token::Tilde => panic!("Unexpected token {:?}", self.peak_token().as_ref().unwrap()),
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
                while let (Token::Int(arg_size), Token::Ident(arg_name)) = (
                    self.next_token().take().unwrap(),
                    self.next_token().take().unwrap(),
                ) {
                    args.push((arg_size as i8, arg_name));
                }

                // consume '{'
                if let Token::LBrace = self.next_token().as_ref().unwrap() {
                    self.next_token();
                } else {
                    panic!(
                        "Expected '{{', got {:?}",
                        self.peak_token().as_ref().unwrap()
                    );
                }
                Stmt::Function(size as i8, name, args, Box::new(self.parse_stmt_block()))
            }
            (size, name) => panic!("Excepted size and name, got {name:?} and {size:?}"),
        }
    }

    fn parse_stmt_block(&mut self) -> Stmt {
        let mut statements = Vec::new();
        while let Some(token) = self.peak_token() {
            match token {
                Token::RParenth => {
                    self.next_token();
                    break;
                }
                _ => {
                    statements.push(self.parse_stmt());
                }
            };
        }
        Stmt::BlockStatement(statements)
    }

    fn parse_stmt_expr(&mut self) -> Stmt {
        let expr = self.parse_expr(Precedence::Lowest);
        Stmt::Expression(expr)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Expr {
        let mut left = self.parse_expr_prefix();
        while precedence < Precedence::from_token(self.peak_token().as_ref().unwrap()) {
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
            Token::At => todo!(),
            Token::Bang => Expr::Not(Box::new(
                self.parse_expr(Precedence::from_token(&Token::Bang)),
            )),
            Token::Float(value) => Expr::Literal(Literal::Float(value)),
            Token::LParenth => self.parse_expr(Precedence::Lowest),
            Token::RParenth => todo!(),
            Token::Plus => todo!(),
            Token::Dash => todo!(),
            Token::Slash => todo!(),
            Token::Modulo => todo!(),
            Token::Asterix => todo!(),
            Token::Equal => todo!(),
            Token::Less => todo!(),
            Token::Colon => todo!(),
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
            Token::Ident(_) => todo!(),
            Token::Int(_) => todo!(),
            Token::At => todo!(),
            Token::Bang => todo!(),
            Token::Float(_) => todo!(),
            Token::LParenth => (self.parse_expr_call(left), true),
            Token::RParenth => todo!(),
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
            Token::Colon => todo!(),
            Token::Question => todo!(),
            Token::Tilde => todo!(),
            Token::LBrace => todo!(),
            Token::RBrace => todo!(),
            // _ => (left, false),
        }
    }

    fn parse_expr_call(&mut self, left: Expr) -> Expr {
        Expr::Call(Box::new(left), self.parse_expr_args())
    }

    fn parse_expr_args(&mut self) -> Vec<Expr> {
        todo!()
    }
}
