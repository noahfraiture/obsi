#![allow(dead_code)]

use core::panic;

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
    Asterix,       // *
    Equal,         // ==
    Bang,          // ! for now
    Less,          // < all comparison can be made with this and equal
    Question,      // ? used in '? condition { print(true) } { print(false) }'
    // TODO: add loop
    At,    // @ declare a function
    Tilde, // ~ return a value
}

pub struct Lexer<'a> {
    current: Option<char>,
    next: Option<char>,
    reader: Box<dyn Iterator<Item = char> + 'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(mut reader: impl Iterator<Item = char> + 'a) -> Self {
        Lexer {
            current: None,
            next: reader.next(),
            reader: Box::new(reader),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current = self.next;
        self.next = self.reader.next();
        self.current
    }

    fn peak_char(&self) -> Option<char> {
        self.next
    }

    /// Give the token starting at next char. End on the final char of the token
    pub fn next_token(&mut self) -> Option<Token> {
        match self.peak_char()? {
            '1'..='9' | 'A'..='F' => self.parse_number(),
            'a'..='z' => self.parse_ident(),
            '(' => {
                self.next_char();
                Some(Token::LParenth)
            }
            ')' => {
                self.next_char();
                Some(Token::RParenth)
            }
            '+' => {
                self.next_char();
                Some(Token::Plus)
            }
            '-' => {
                self.next_char();
                Some(Token::Dash)
            }
            '/' => {
                self.next_char();
                Some(Token::Slash)
            }
            '%' => {
                self.next_char();
                Some(Token::Modulo)
            }
            '*' => {
                self.next_char();
                Some(Token::Asterix)
            }
            '=' => {
                if let Some('=') = self.peak_char() {
                    self.next_char();
                    Some(Token::Equal)
                } else {
                    // we don't use an assign to assign a value
                    panic!("Unexpected character {:?}", self.current.unwrap())
                }
            }
            '!' => {
                self.next_char();
                Some(Token::Bang)
            }
            '<' => {
                self.next_char();
                Some(Token::Less)
            }
            '?' => {
                self.next_char();
                Some(Token::Question)
            }
            '@' => {
                self.next_char();
                Some(Token::At)
            }
            '~' => {
                self.next_char();
                Some(Token::Tilde)
            }
            '{' => {
                self.next_char();
                Some(Token::LBrace)
            }
            '}' => {
                self.next_char();
                Some(Token::RBrace)
            }
            ' ' | '\t' | '\n' | '\r' => {
                self.next_char();
                self.next_token()
            }
            _ => panic!("Unexpected character {:?}", self.current.unwrap()),
        }
    }

    pub fn peak_token(&mut self) -> Option<Token> {
        todo!()
    }

    // pub function that says if there is any token left
    pub fn has_next(&self) -> bool {
        self.next.is_some()
    }

    // TEST: point and hex can be complicated and accept unwanted result
    fn parse_number(&mut self) -> Option<Token> {
        let mut num = self.next_char().unwrap().to_string();
        let mut is_float = false;
        let mut is_hex = false;
        while let Some(next) = self.peak_char() {
            match next {
                '1'..='9' => {
                    num.push(self.next_char()?);
                }
                'A'..='F' if !is_float => {
                    num.push(self.next_char()?);
                    is_hex = true;
                }
                '.' if !is_float && !is_hex => {
                    is_float = true;
                    num.push(self.next_char()?);
                }
                _ => break,
            }
        }
        if is_float {
            Some(Token::Float(num.parse().unwrap()))
        } else {
            Some(Token::Int(i64::from_str_radix(&num, 16).unwrap()))
        }
    }

    fn parse_ident(&mut self) -> Option<Token> {
        let mut ident = self.next_char().unwrap().to_string();
        while let Some(next) = self.peak_char() {
            if next.is_ascii_lowercase() {
                ident.push(self.next_char()?);
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
    ~ d
}"#;
        let mut lexer = Lexer::new(input.chars());

        let expected_tokens = vec![
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
            Token::Tilde,
            Token::Ident("d".to_string()),
            Token::RBrace,
        ];

        for expected_token in expected_tokens {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }
}
