#![allow(dead_code)]

use core::panic;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String), // foo
    Int(i64),      // 3.4, 1A
    Float(f64),    // NOTE : could probably be removed if we work with byte only
    LParenth,      // (
    RParenth,      // )
    LBrace,        // {
    RBrace,        // }
    Plus,          // +
    Dash,          // -
    Slash,         // /
    Modulo,        // %
    Asterix,       // * times
    Equal,         // ==
    Bang,          // ! not at bit level, thus !8 = -9
    Xor,           // ^
    Or,            // |
    And,           // & 'and'
    Comma,         // , deref
    Backtick,      // ` pointer
    P,             // P pointer type
    Less,          // < all comparison can be made with this and equal

    // could replace '} {', but must find a unique character to represent it
    Question, // ? used in '? condition { print(true) } { print(false) }'
    At,       // @ declare a function. '@ main {}'
    Dollar,   // $ call a function.    '$ add a b'
    Tilde,    // ~ return a value.     '~ result'

              // TODO: add loop
}

pub struct Lexer<'a> {
    reader: Peekable<Box<dyn Iterator<Item = char> + 'a>>,
}

// NOTE : we might replace some symbol by upper case after F
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        match self.reader.peek()? {
            '1'..='9' | 'A'..='F' => self.parse_number(),
            'a'..='z' => self.parse_ident(),
            '(' => {
                self.reader.next();
                Some(Token::LParenth)
            }
            ')' => {
                self.reader.next();
                Some(Token::RParenth)
            }
            '+' => {
                self.reader.next();
                Some(Token::Plus)
            }
            '-' => {
                self.reader.next();
                Some(Token::Dash)
            }
            '/' => {
                self.reader.next();
                Some(Token::Slash)
            }
            '%' => {
                self.reader.next();
                Some(Token::Modulo)
            }
            '*' => {
                self.reader.next();
                Some(Token::Asterix)
            }
            '=' => {
                if let Some('=') = self.reader.peek() {
                    self.reader.next();
                    Some(Token::Equal)
                } else {
                    // we don't use an assign to assign a value
                    panic!("Unexpected character {:?}", self.reader.peek())
                }
            }
            '!' => {
                self.reader.next();
                Some(Token::Bang)
            }
            '^' => {
                self.reader.next();
                Some(Token::Xor)
            }
            '|' => {
                self.reader.next();
                Some(Token::Or)
            }
            '&' => {
                self.reader.next();
                Some(Token::And)
            }
            '<' => {
                self.reader.next();
                Some(Token::Less)
            }
            '?' => {
                self.reader.next();
                Some(Token::Question)
            }
            '@' => {
                self.reader.next();
                Some(Token::At)
            }
            '$' => {
                self.reader.next();
                Some(Token::Dollar)
            }
            '~' => {
                self.reader.next();
                Some(Token::Tilde)
            }
            '{' => {
                self.reader.next();
                Some(Token::LBrace)
            }
            '}' => {
                self.reader.next();
                Some(Token::RBrace)
            }
            ',' => {
                self.reader.next();
                Some(Token::Comma)
            }
            '`' => {
                self.reader.next();
                Some(Token::Backtick)
            }
            'P' => {
                self.reader.next();
                Some(Token::P)
            }
            ' ' | '\t' | '\n' | '\r' => {
                self.reader.next();
                self.next()
            }
            _ => panic!("Unexpected character {:?}", self.reader.peek()), // unwrap in a panic does not really make sense
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(reader: impl Iterator<Item = char> + 'a) -> Self {
        let x: Box<dyn Iterator<Item = char> + 'a> = Box::new(reader);
        Lexer {
            reader: x.peekable(),
        }
    }

    // pub function that says if there is any token left
    pub fn has_next(&mut self) -> bool {
        self.reader.peek().is_some()
    }

    // TEST: point and hex can be complicated and accept unwanted result
    fn parse_number(&mut self) -> Option<Token> {
        let chars: String = self
            .reader
            .by_ref()
            .take_while(|c| matches!(c, '1'..='9' | '.' | 'A'..='F'))
            .collect();

        if chars.contains(".") {
            chars.parse().ok().map(Token::Float)
        } else {
            i64::from_str_radix(&chars, 16).ok().map(Token::Int)
        }
    }

    fn parse_ident(&mut self) -> Option<Token> {
        let mut ident = self.reader.next().unwrap().to_string();
        while let Some(next) = self.reader.peek() {
            if next.is_ascii_lowercase() {
                ident.push(self.reader.next()?);
            } else {
                break;
            }
        }
        Some(Token::Ident(ident))
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
    ptr ,five
    deref `ptr
    ~ d
}"#;
        let mut lexer = Lexer::new(input.chars());

        let expected_tokens = [
            Token::At,
            Token::Int(4),
            Token::Ident("compute".to_string()),
            Token::Int(4),
            Token::Ident("a".to_string()),
            Token::Int(4),
            Token::Ident("b".to_string()),
            Token::LBrace,
            Token::Int(4),
            Token::Ident("five".to_string()),
            Token::Int(4),
            Token::Ident("c".to_string()),
            Token::Int(4),
            Token::Ident("d".to_string()),
            Token::Ident("five".to_string()),
            Token::Int(5),
            Token::Ident("c".to_string()),
            Token::Ident("a".to_string()),
            Token::Plus,
            Token::Ident("b".to_string()),
            Token::Question,
            Token::Ident("b".to_string()),
            Token::Less,
            Token::Int(5),
            Token::LBrace,
            Token::Ident("d".to_string()),
            Token::Ident("a".to_string()),
            Token::Plus,
            Token::Int(5),
            Token::RBrace,
            Token::LBrace,
            Token::Ident("d".to_string()),
            Token::Ident("a".to_string()),
            Token::Dash,
            Token::Int(5),
            Token::RBrace,
            Token::P,
            Token::Ident("ptr".to_string()),
            Token::Int(4),
            Token::Ident("deref".to_string()),
            Token::Ident("ptr".to_string()),
            Token::Comma,
            Token::Ident("five".to_string()),
            Token::Ident("deref".to_string()),
            Token::Backtick,
            Token::Ident("ptr".to_string()),
            Token::Tilde,
            Token::Ident("d".to_string()),
            Token::RBrace,
        ];

        for expected_token in expected_tokens {
            assert_eq!(lexer.next().unwrap(), expected_token);
        }
    }
}
