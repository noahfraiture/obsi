#![allow(dead_code)]

#[derive(Debug)]
pub enum Token {
    Ident(String),
    Int(i64),
    Float(f64), // NOTE : could probably be removed if we work with byte only
    LBracket,
    RBracket,
    LParenth,
    RParenth,
    Plus,
    Dash,
    Slash,
    Modulo,
    Assign,
    Asterix,
    Semicolon,
    DoubleQuote,
    Equal,
    Bang,
    Less, // all other comparator can be made from this
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
        let token = match self.peak_char()? {
            '1'..='9' | 'A'..='F' => self.parse_number(),
            'a'..='z' => self.parse_ident(),
            '(' => Some(Token::LParenth),
            ')' => Some(Token::RParenth),
            '{' => Some(Token::LBracket),
            '}' => Some(Token::RBracket),
            '+' => Some(Token::Plus),
            '-' => Some(Token::Dash),
            '/' => Some(Token::Slash),
            '%' => Some(Token::Modulo),
            '*' => Some(Token::Asterix),
            ';' => Some(Token::Semicolon),
            '=' => {
                if let Some('=') = self.peak_char() {
                    self.next_char();
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }
            '!' => Some(Token::Bang),
            '<' => Some(Token::Less),
            '"' => Some(Token::DoubleQuote),
            '\n' | '\r' | '\t' => self.next_token(),
            _ => panic!("Unexpected character {:?}", self.current.unwrap()),
        };
        self.next_char();
        token
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
