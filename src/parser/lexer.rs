use std::iter::Peekable;
use std::str::Chars;

use crate::parser;
use crate::{IdentType, OperatorType, Token};

use std::rc::Rc;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_is_ident_char(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => parser::is_ident_char(ch),
            None => false,
        }
    }

    fn peek_is_numeric_char(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => ch.is_numeric() || ch == '.',
            None => false,
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::with_capacity(6);
        ident.push(first);

        while self.peek_is_ident_char() {
            ident.push(self.read_char().unwrap());
        }

        ident
    }

    fn read_string_literal(&mut self) -> String {
        let mut ret_str = String::new();

        let mut ch = self.read_char().unwrap();
        while ch != '\"' {
            ret_str.push(ch);
            ch = self.read_char().unwrap();
        }

        ret_str
    }

    fn read_num(&mut self, first: char) -> String {
        let mut literal = String::with_capacity(5);
        literal.push(first);

        while self.peek_is_numeric_char() {
            literal.push(self.read_char().unwrap());
        }

        literal
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.peek_char() {
            if !ch.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some(';') => Token::Semicolon,
            Some(':') => Token::Colon,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('+') => Token::Operator(OperatorType::Plus),
            Some('-') => {
                if self.peek_char().unwrap() == &'>' {
                    self.read_char();
                    Token::RetArrow
                } else {
                    Token::Operator(OperatorType::Minus)
                }
            }
            Some('/') => Token::Operator(OperatorType::Divide),
            Some('*') => Token::Operator(OperatorType::Multiply),
            Some('^') => Token::Operator(OperatorType::Exponentiate),
            Some('%') => Token::Operator(OperatorType::Modulus),
            Some('!') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Operator(OperatorType::NotEqual)
                } else {
                    panic!("invalid !")
                }
            }
            Some('>') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Operator(OperatorType::GreaterEqual)
                } else {
                    Token::Operator(OperatorType::Greater)
                }
            }
            Some('<') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Operator(OperatorType::LessEqual)
                } else {
                    Token::Operator(OperatorType::Less)
                }
            }
            Some('=') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Operator(OperatorType::Equal)
                } else {
                    Token::Assign
                }
            }
            Some('\"') => Token::StringLiteral(Rc::new(self.read_string_literal())),
            Some(ch) => {
                if ch.is_numeric() {
                    let read_number = self.read_num(ch);

                    if let Ok(int) = read_number.parse::<isize>() {
                        Token::Integer(int)
                    } else if let Ok(float) = read_number.parse::<f64>() {
                        Token::Float(float)
                    } else {
                        Token::Illegal
                    }
                } else if parser::is_ident_char(ch) {
                    let ident = self.read_identifier(ch);
                    match ident.as_str() {
                        "and" => Token::Operator(OperatorType::And),
                        "or" => Token::Operator(OperatorType::Or),
                        "as" => Token::Operator(OperatorType::Cast),
                        "print" => Token::Ident(IdentType::Print),
                        "fn" => Token::Ident(IdentType::Function),
                        "let" => Token::Ident(IdentType::Let),
                        "if" => Token::Ident(IdentType::If),
                        "else" => Token::Ident(IdentType::Else),
                        "elif" => Token::Ident(IdentType::Elif),
                        "return" => Token::Ident(IdentType::Return),
                        "while" => Token::While,
                        _ => Token::Name(Rc::new(ident)),
                    }
                } else {
                    Token::Illegal
                }
            }
            _ => Token::EOF,
        }
    }
}
