use std::io::{self, BufRead, Write};

use std::iter::Peekable;
use std::str::Chars;

use std::collections::HashMap;

mod parser;
use parser::{Expression, SubExpression};

mod tests;

#[derive(Clone, Debug, PartialEq)]
pub struct SlangFn {
    args: Vec<String>,
    block: Vec<BlockSection>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockSection {
    Line(Vec<Token>),
    InnerBlock(Vec<BlockSection>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Variable {
    Integer(isize),
    Float(f64),
    Str(String),
    Bool(bool),
    Function(SlangFn),
    Type(String),
    Custom(String, HashMap<String, Variable>),
}

impl Variable {
    pub fn same_type(&self, rhs: &Variable) -> bool {
        match (self, rhs) {
            (&Variable::Integer(_), &Variable::Integer(_)) => true,
            (&Variable::Float(_), &Variable::Float(_)) => true,
            (&Variable::Str(_), &Variable::Str(_)) => true,
            (&Variable::Custom(_, _), &Variable::Custom(_, _)) => {
                let self_clone = self.clone();
                let rhs_clone = rhs.clone();
                match (self_clone, rhs_clone) {
                    (Variable::Custom(t1, _), Variable::Custom(t2, _)) => t1 == t2,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Semicolon,
    While,
    LBrace,
    RBrace,
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
    Operator(OperatorType),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum IdentType {
    Print,
    Function,
    Let,
    If,
    Else,
    Return,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum OperatorType {
    Cast,
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiate,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Equal,
}

impl PartialOrd for OperatorType {
    fn partial_cmp(&self, _rhs: &OperatorType) -> Option<std::cmp::Ordering> {
        None
    }
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
        let mut ident = String::with_capacity(10);
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

    fn read_expr2(tokens: &[Token]) -> parser::Expression {
        let mut stack: Vec<Token> = Vec::new();
        stack.push(Token::LParen);
        let mut postfix_expr = Expression::new();
        let mut i = 0;

        for token in tokens.iter().cloned() {
            match token {
                Token::LParen => stack.push(token),

                Token::Integer(_) | Token::Float(_) | Token::Name(_) | Token::StringLiteral(_) => {
                    match token {
                        Token::Name(name) => postfix_expr.insert(i, SubExpression::Name(name)),
                        Token::Integer(int) => {
                            postfix_expr.insert(i, SubExpression::Val(Variable::Integer(int)))
                        }
                        Token::Float(float) => {
                            postfix_expr.insert(i, SubExpression::Val(Variable::Float(float)))
                        }
                        Token::StringLiteral(string) => {
                            postfix_expr.insert(i, SubExpression::Val(Variable::Str(string)))
                        }
                        _ => panic!(),
                    }
                    i += 1;
                }

                Token::Operator(token_ty) => {
                    use parser::operator_precedence;
                    let mut last_val = stack.pop().unwrap();
                    while let Token::Operator(stack_ty) = last_val {
                        if !operator_precedence(stack_ty) > operator_precedence(token_ty) {
                            break;
                        }
                        postfix_expr.insert(i, SubExpression::Operator(stack_ty));
                        i += 1;
                        last_val = stack.pop().unwrap();
                    }
                    stack.push(last_val);
                    stack.push(token);
                }

                Token::RParen | Token::EOF | Token::LBrace | Token::Semicolon => {
                    let mut last_val = stack.pop().unwrap();
                    while last_val != Token::LParen {
                        postfix_expr.insert(i, parser::token_to_subexpr(last_val));
                        i += 1;
                        last_val = stack.pop().unwrap();
                    }
                    break;
                }

                _ => {
                    dbg!(token);
                    todo!()
                }
            }
        }

        postfix_expr
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some(';') => Token::LBrace,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
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
            Some('\"') => Token::StringLiteral(self.read_string_literal()),
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
                } else if is_ident_char(ch) {
                    let ident = self.read_identifier(ch);
                    match ident.as_str() {
                        "as" => Token::Operator(OperatorType::Cast),
                        "print" => Token::Ident(IdentType::Print),
                        "fn" => Token::Ident(IdentType::Function),
                        "let" => Token::Ident(IdentType::Let),
                        "if" => Token::Ident(IdentType::If),
                        "else" => Token::Ident(IdentType::Else),
                        "return" => Token::Ident(IdentType::Return),
                        "while" => Token::While,
                        _ => Token::Name(ident),
                    }
                } else {
                    Token::Illegal
                }
            }
            _ => Token::EOF,
        }
    }

    #[allow(clippy::useless_let_if_seq)]
    pub fn read_next_block(reader: &mut Option<impl Iterator<Item = Result<String, std::io::Error>> + 'a>) -> Option<Vec<String>> {
        let mut ret_block: Vec<String> = Vec::new();
        let mut brace_cnt = 0u16;
        let mut end_file = false;
        loop {
            let mut line = String::new();
            match reader {
                Some(lines) => {
                    line = if let Some(Ok(ln)) = lines.next() {
                        ln.to_string()
                    } else {
                        end_file = true;
                        break;
                    }
                },
                None => {io::stdin().lock().read_line(&mut line).unwrap();},
            }

            let mut contains_brace = false;
            if line.contains('{') {
                brace_cnt += 1;
                contains_brace = true;
            }
            if line.contains('}') {
                brace_cnt -= 1;
                if brace_cnt == 0 {
                    ret_block.push(line);
                    break;
                }
            }

            ret_block.push(line);
            if !contains_brace && brace_cnt == 0 {
                break;
            }
        }

        if end_file {
            None
        } else {
            Some(ret_block)
        }
    }
}

fn process_block(in_block: &[&str]) -> Vec<BlockSection> {
    let mut blocks: Vec<BlockSection> = Vec::new();

    let mut line_iterator = in_block.iter();
    let mut line_num = 0;
    while let Some(block_line) = line_iterator.next() {
        let mut lexer = Lexer::new(&block_line);
        let mut line: Vec<Token> = Vec::new();

        loop {
            let token = lexer.next_token();
            if token == Token::EOF {
                if !line.is_empty() {
                    line.push(Token::EOF);
                    blocks.push(BlockSection::Line(line.clone()));
                }
                break;
            }

            if token != Token::LBrace && token != Token::RBrace {
                line.push(token);
            } else if token == Token::LBrace {
                line.push(Token::EOF);
                blocks.push(BlockSection::Line(line.clone()));

                let mut brace_cnt = 1;
                let mut num_lines = 0;
                let mut next_line = line_iterator.next();
                while let Some(line) = next_line {
                    if line.contains('{') {
                        brace_cnt += 1;
                    }
                    if line.contains('}') {
                        brace_cnt -= 1;
                    }
                    if brace_cnt == 0 {
                        break;
                    }

                    num_lines += 1;
                    next_line = line_iterator.next();
                }
                let block = process_block(&in_block[line_num + 1..line_num + 1 + num_lines]);

                blocks.push(BlockSection::InnerBlock(block));

                line = Vec::new();
            } else if token == Token::RBrace {
                break;
            }
        }

        line_num += 1;
    }
    // dbg!(blocks.clone());

    blocks
}

fn exec_block(block: Vec<BlockSection>, vars: &mut HashMap<String, Variable>) {
    let mut block_iter = block.into_iter();
    while let Some(block_section) = block_iter.next() {
        match block_section {
            BlockSection::Line(tokens) => {
                let mut token_iter = tokens.iter().peekable();
                while let Some(token) = token_iter.next() {
                    match token {
                        Token::While => {
                            let conditional_expr = Lexer::read_expr2(
                                token_iter
                                .clone()
                                .cloned()
                                .collect::<Vec<Token>>()
                                .as_slice(),
                            );
                            let inner_block = block_iter.next();

                            let mut inner_vars = vars.clone();

                            if let BlockSection::InnerBlock(inner_block_vec) = inner_block.unwrap()
                            {
                                while parser::eval_expr(&conditional_expr, &inner_vars)
                                    == Variable::Bool(true)
                                    {
                                        exec_block(inner_block_vec.to_vec(), &mut inner_vars);
                                    }
                            }

                            for (k, v) in inner_vars {
                                if let Some(old_v) = vars.get_mut(&k) {
                                    *old_v = v;
                                }
                            }
                            break;
                        }

                        Token::Ident(ty) => match ty {
                            IdentType::Let => match token_iter.next().unwrap() {
                                Token::Name(lhs) => {
                                    if let Token::Assign = token_iter.next().unwrap() {
                                        let rhs_expr = Lexer::read_expr2(
                                            token_iter.cloned().collect::<Vec<Token>>().as_slice(),
                                        );
                                        let rhs_eval = parser::eval_expr(&rhs_expr, &vars);
                                        vars.insert(lhs.to_string(), rhs_eval);
                                        break;
                                    } else {
                                        panic!("syntax error after let token")
                                    }
                                }
                                _ => panic!("invalid assign"),
                            },
                            IdentType::Print => {
                                let expr = Lexer::read_expr2(
                                    token_iter.cloned().collect::<Vec<Token>>().as_slice(),
                                );
                                println!("{:?}", parser::eval_expr(&expr, &vars));
                                break;
                            }
                            _ => todo!(),
                        },

                        // Token::LParen => stack.push(token),
                        Token::Integer(_)
                            | Token::Float(_)
                            | Token::Name(_)
                            | Token::StringLiteral(_)
                            | Token::Operator(_)
                            | Token::RParen
                            | Token::EOF
                            | Token::LParen => {
                                if let Some(next_token) = token_iter.next() {
                                    if let (Token::Name(lhs), Token::Assign) =
                                        (token.clone(), next_token.clone())
                                    {
                                        if !vars.contains_key(&lhs) {
                                            panic!("Assigned to uninitialized variable {}", lhs);
                                        }
                                        let rhs_expr = Lexer::read_expr2(
                                            token_iter.cloned().collect::<Vec<Token>>().as_slice(),
                                        );
                                        let rhs_eval = parser::eval_expr(&rhs_expr, &vars);

                                        let prev_val = vars.get(&lhs).unwrap();
                                        if prev_val.same_type(&rhs_eval) {
                                            vars.insert(lhs, rhs_eval);
                                        } else {
                                            panic!(
                                                "Variable types do not match: {:?}, {:?}",
                                                lhs, rhs_eval
                                            );
                                        }
                                        break;
                                    }
                                }
                                break;
                            }

                        _ => todo!(),
                    }
                }
            }
            BlockSection::InnerBlock(blocks) => {
                exec_block(blocks, vars);
            }
        }
    }
}

// fn run_line(mut in_line: String, vars: &mut HashMap<String, Variable>) {
//     let mut lexer = Lexer::new(&mut in_line);

//     loop {
//         let token = lexer.next_token();

//         match token {
//             Token::While => {
//                 let conditional_expr = lexer.read_expr(vec![]);
//                 let block = lexer.read_block();

//                 while parser::eval_expr(&conditional_expr, &vars) == Variable::Bool(true) {
//                     run_block(&block, vars);
//                 }
//             }

//             Token::Ident(ty) => match ty {
//                 IdentType::Let => match lexer.next_token() {
//                     Token::Name(lhs) => {
//                         if let Token::Assign = lexer.next_token() {
//                             let rhs_expr = lexer.read_expr(vec![]);
//                             let rhs_eval = parser::eval_expr(&rhs_expr, &vars);
//                             vars.insert(lhs, rhs_eval);
//                             break;
//                         } else {
//                             panic!("syntax error after let token")
//                         }
//                     }
//                     _ => panic!("invalid assign"),
//                 },
//                 _ => todo!(),
//             },

//             // Token::LParen => stack.push(token),
//             Token::Integer(_)
//                 | Token::Float(_)
//                 | Token::Name(_)
//                 | Token::StringLiteral(_)
//                 | Token::Operator(_)
//                 | Token::RParen
//                 | Token::EOF
//                 | Token::LParen => {
//                     lexer.skip_whitespace();
//                     let next_token = lexer.next_token();
//                     if let (Token::Name(lhs), Token::Assign) = (token.clone(), next_token.clone()) {
//                         if !vars.contains_key(&lhs) {
//                             panic!("Assigned to uninitialized variable {}", lhs);
//                         }
//                         let rhs_expr = lexer.read_expr(vec![]);
//                         let rhs_eval = parser::eval_expr(&rhs_expr, &vars);

//                         let prev_val = vars.get(&lhs).unwrap();
//                         if prev_val.same_type(&rhs_eval) {
//                             vars.insert(lhs, rhs_eval);
//                         } else {
//                             panic!("Variable types do not match: {:?}, {:?}", lhs, rhs_eval);
//                         }
//                         break;
//                     }

//                     let postfix_expr = lexer.read_expr(vec![token, next_token]);
//                     if postfix_expr.len() > 0 {
//                         println!("{:?}", parser::eval_expr(&postfix_expr, &vars));
//                     }
//                     break;
//                 }

//             _ => todo!(),
//         }
//     }
// }

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn main() {
    use std::io::BufReader;
    let args = std::env::args().collect::<Vec<String>>();
    let filename = args.get(1);
    let mut reader_lines = filename.map(|name| BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines());
    let mut vars: HashMap<String, Variable> = HashMap::new();
    vars.insert("PI".to_owned(), Variable::Float(std::f64::consts::PI));
    vars.insert("String".to_owned(), Variable::Type("String".to_owned()));
    vars.insert("Int".to_owned(), Variable::Type("Int".to_owned()));
    vars.insert("Float".to_owned(), Variable::Type("Float".to_owned()));
    vars.insert("Bool".to_owned(), Variable::Type("Bool".to_owned()));

    loop {
        if filename.is_none() {
            print!("> ");
        }
        io::stdout().flush().unwrap();

        let block_res = Lexer::read_next_block(&mut reader_lines);
        match block_res {
            Some(block) => {
                let block = process_block(
                    block
                    .iter()
                    .map(|string| string.as_str())
                    .collect::<Vec<&str>>()
                    .as_slice(),
                );
                exec_block(block, &mut vars);
            },
            None => break,
        }
        // run_line(line, &mut vars);
    }
}
