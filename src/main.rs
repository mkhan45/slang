use std::io::{self, BufRead, Write};

use std::convert::TryInto;

use std::iter::Peekable;
use std::str::Chars;

use std::collections::HashMap;

mod parser;
use parser::{SubExpression, Expression};

#[derive(Clone, Debug, PartialEq)]
pub enum Variable {
    Integer(isize),
    Float(f64),
    Str(String),
    Custom(HashMap<String, Variable>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Assign,
    Illegal,
    EOF,
    Ident(IdentType),
    Integer(isize),
    Float(f64),
    StringLiteral(String),
    Name(String),
    Comparison(ComparisonType),
    Operator(OperatorType),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum IdentType {
    Function,
    Let,
    If,
    Else,
    Return,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ComparisonType {
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Equal,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OperatorType {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiate,
}

struct Lexer<'a> {
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
            Some(&ch) => is_ident_char(ch),
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
        let mut ident = String::new();
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
        let mut literal = String::new();
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

    fn read_expr(&mut self, first_token: Token, vars: &HashMap<String, Variable>) -> parser::Expression {
        let mut stack: Vec<Token> = Vec::new();
        stack.push(Token::LParen);

        let mut postfix_expr = Expression::new();

        let mut j = 0;

        let mut used_first = false;

        loop {
            let token = if used_first {
                self.next_token()
            } else {
                used_first = true;
                first_token.clone()
            };

            match token {
                Token::LParen => stack.push(token),

                Token::Integer(_) | Token::Float(_) | Token::Name(_) | Token::StringLiteral(_) => {
                    match token {
                        Token::Name(name) => postfix_expr.insert(j, SubExpression::Val(vars.get(&name).unwrap().clone())),
                        Token::Integer(int) => postfix_expr.insert(j, SubExpression::Val(Variable::Integer(int))),
                        Token::Float(float) => postfix_expr.insert(j, SubExpression::Val(Variable::Float(float))),
                        Token::StringLiteral(string) => postfix_expr.insert(j, SubExpression::Val(Variable::Str(string))),
                        _ => panic!(),
                    }
                    j += 1;
                },

                Token::Operator(token_ty) => {
                    use parser::operator_precedence;
                    let mut last_val = stack.pop().unwrap();
                    while let Token::Operator(stack_ty) = last_val {
                        if ! operator_precedence(stack_ty) > operator_precedence(token_ty) {
                            break;
                        }
                        postfix_expr.insert(j, SubExpression::Operator(stack_ty));
                        j += 1;
                        last_val = stack.pop().unwrap();
                    }
                    stack.push(last_val);
                    stack.push(token);
                },

                Token::RParen | Token::EOF => {
                    let mut last_val = stack.pop().unwrap();
                    while last_val != Token::LParen {
                        postfix_expr.insert(j, parser::token_to_subexpr(last_val));
                        j += 1;
                        last_val = stack.pop().unwrap();
                    }
                    break postfix_expr;
                },

                _ => todo!(),
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('+') => Token::Operator(OperatorType::Plus),
            Some('-') => Token::Operator(OperatorType::Minus),
            Some('/') => Token::Operator(OperatorType::Divide),
            Some('*') => Token::Operator(OperatorType::Multiply),
            Some('^') => Token::Operator(OperatorType::Exponentiate),
            Some('>') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Comparison(ComparisonType::GreaterEqual)
                } else {
                    Token::Comparison(ComparisonType::Greater)
                }
            },
            Some('<') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Comparison(ComparisonType::LessEqual)
                } else {
                    Token::Comparison(ComparisonType::Less)
                }
            },
            Some('=') => {
                if self.peek_char().unwrap() == &'=' {
                    self.read_char();
                    Token::Comparison(ComparisonType::Equal)
                } else {
                    Token::Assign
                }
            }
            Some('\"') => {
                Token::StringLiteral(self.read_string_literal())
            },
            Some(ch @ _) => {
                if ch.is_numeric() {
                    let read_number = self.read_num(ch);

                    if let Ok(int) = read_number.parse::<isize>() {
                        Token::Integer(int)
                    } else if let Ok(float) = read_number.parse::<f64>() {
                        Token::Float(float)
                    } else {
                        Token::Illegal
                    }
                } else if is_ident_char(ch) {
                    let ident = self.read_identifier(ch);
                    match ident.as_str() {
                        "fn" => Token::Ident(IdentType::Function),
                        "let" => Token::Ident(IdentType::Let),
                        "If" => Token::Ident(IdentType::If),
                        "Else" => Token::Ident(IdentType::Else),
                        "return" => Token::Ident(IdentType::Return),
                        _ => Token::Name(ident),
                    }
                } else {
                    Token::Illegal
                }
            }
            _ => Token::EOF,
        }
    }
}

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn main() {
    let stdin = io::stdin();

    let mut vars: HashMap<String, Variable> = HashMap::new();
    vars.insert("PI".to_owned(), Variable::Float(std::f64::consts::PI));

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        stdin.lock().read_line(&mut line).unwrap();

        let mut lexer = Lexer::new(&mut line);

        loop {
            let token = lexer.next_token();

            match token {
                Token::Ident(ty) => {
                    match ty {
                        IdentType::Let => {
                            match lexer.next_token() {
                                Token::Name(lhs) => {
                                    if let Token::Assign = lexer.next_token() {
                                        let token = lexer.next_token();
                                        let rhs_expr = lexer.read_expr(token, &vars);
                                        let rhs_eval = parser::eval_expr(rhs_expr, &vars);
                                        vars.insert(lhs, rhs_eval);
                                        break;
                                    } else {
                                        panic!("syntax error after let token")
                                    }
                                },
                                _ => panic!("invalid assign"),
                            }
                        },
                        _ => todo!(),
                    }
                }


                // Token::LParen => stack.push(token),
                
                Token::Integer(_) | Token::Float(_) | Token::Name(_) | Token::StringLiteral(_) | Token::Operator(_) |
                    Token::RParen | Token::EOF | Token::LParen => {
                        let postfix_expr = lexer.read_expr(token, &vars);
                        println!("{:?}", parser::eval_expr(postfix_expr, &vars));
                        break;
                    }

                _ => todo!(),
            }
        }

    }
}
