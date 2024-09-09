use crate::lexer::Token;

pub struct Program(pub Vec<Stmt>);

pub enum Stmt {
    Declare(i8, String),                                // <size> <ident>
    Assignment(String, Expr),                           // <ident> <expr>
    Expression(Expr),                                   // <expr>
    Function(i8, String, Vec<(i8, String)>, Box<Stmt>), // <size> <ident> <args>* <body>
    Return(Expr),                                       // <expr>
    BlockStatement(Vec<Stmt>),                          // <stmt>*
}

pub enum Expr {
    Literal(Literal),                      // <literal>
    Variable(String),                      // <ident>
    BinaryOp(Box<Expr>, BinOp, Box<Expr>), // <expr> <binop> <expr>
    Not(Box<Expr>),                        // ! <expr>
    Call(Box<Expr>, Vec<Expr>),            // <expr> <expr>* == <function> <args>*
}

pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Equal,
}

#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest = 0,
    Equals,
    Less,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    pub fn from_token(token: &Token) -> Precedence {
        match token {
            Token::LParenth => Self::Call,
            Token::Plus => Self::Sum,
            Token::Dash => Self::Sum,
            Token::Slash => Self::Product,
            Token::Modulo => Self::Product,
            Token::Asterix => Self::Product,
            Token::Equal => Self::Equals,
            Token::Bang => Self::Prefix,
            Token::Less => Self::Less,
            _ => panic!("Unknown precedence"),
        }
    }
}
