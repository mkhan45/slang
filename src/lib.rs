use std::iter::Peekable;
use std::str::Chars;

use std::io::{self, BufRead};

use std::collections::HashMap;

use std::rc::Rc;

mod parser;
use parser::{Expression, SubExpression};

pub mod tests;

#[macro_export]
macro_rules! default_vars {
    () => {{
        use std::rc::Rc;
        let mut vars: HashMap<String, Variable> = HashMap::new();
        vars.insert("PI".to_owned(), Variable::Float(std::f64::consts::PI));
        vars.insert(
            "String".to_owned(),
            Variable::Type(Rc::new("String".to_owned())),
        );
        vars.insert("Int".to_owned(), Variable::Type(Rc::new("Int".to_owned())));
        vars.insert(
            "Float".to_owned(),
            Variable::Type(Rc::new("Float".to_owned())),
        );
        vars.insert(
            "Bool".to_owned(),
            Variable::Type(Rc::new("Bool".to_owned())),
        );
        vars.insert("True".to_owned(), Variable::Bool(true));
        vars.insert("False".to_owned(), Variable::Bool(false));
        vars
    }};
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct InterpreterContext {
    if_else_status: Vec<bool>,
    ret_val: Option<Variable>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct SlangFn {
    args: Vec<(Rc<String>, VarType)>,
    block: Vec<BlockSection>,
    ret_type: Option<VarType>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum BlockSection {
    Line(Vec<Token>),
    InnerBlock(Vec<BlockSection>),
}

type VarType = Rc<String>;

#[derive(Debug, PartialEq, Clone)]
pub enum Variable {
    Null,
    Integer(isize),
    Float(f64),
    Str(Rc<String>),
    Bool(bool),
    Function(Rc<SlangFn>),
    Type(VarType),
    Custom(Rc<(String, HashMap<String, Variable>)>),
}

impl Variable {
    pub fn same_type(&self, rhs: &Variable) -> bool {
        match (self, rhs) {
            (&Variable::Integer(_), &Variable::Integer(_)) => true,
            (&Variable::Float(_), &Variable::Float(_)) => true,
            (&Variable::Str(_), &Variable::Str(_)) => true,
            (&Variable::Bool(_), &Variable::Bool(_)) => true,
            (&Variable::Custom(_), &Variable::Custom(_)) => match (self, rhs) {
                (Variable::Custom(box1), Variable::Custom(box2)) => *box1.0 == *box2.0,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    RetArrow,
    Colon,
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
    StringLiteral(Rc<String>),
    Name(Rc<String>),
    Operator(OperatorType),
}

#[derive(Clone, Copy, PartialEq, Debug, PartialOrd)]
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
    NotEqual,
    Greater,
    Less,
    Equal,
    Modulus,
    And,
    Or,
}

impl PartialOrd for OperatorType {
    fn partial_cmp(&self, _rhs: &OperatorType) -> Option<std::cmp::Ordering> {
        None
    }
}

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

    fn read_expr(token_iter: &mut impl Iterator<Item = &'a Token>, vars: &HashMap<String, Variable>) -> parser::Expression {
        let mut stack: Vec<Token> = Vec::with_capacity(8);
        stack.push(Token::LParen);
        let mut postfix_expr = Expression::new();

        while let Some(token) = token_iter.next() {
            match token {
                Token::LParen => stack.push(token.clone()),

                Token::Integer(_) | Token::Float(_) | Token::Name(_) | Token::StringLiteral(_) => {
                    match token {
                        Token::Name(name) => {
                            if let Some(Variable::Function(slang_fn_rc)) = vars.get(name.as_ref()) {
                                let args_name_iter = slang_fn_rc.as_ref().args.iter().map(|(k, _)| k);
                                let mut args_map: HashMap<String, Vec<SubExpression>> = HashMap::with_capacity(args_name_iter.len());

                                if let Some(Token::LParen) = token_iter.next() {
                                    for var_name in args_name_iter {
                                        args_map.insert(var_name.to_string(), Lexer::read_expr(token_iter, vars));
                                    }
                                }


                                let block = &slang_fn_rc.as_ref().block;
                                args_map.insert(name.to_string(), vec![SubExpression::Val(Variable::Function(slang_fn_rc.clone()))]);

                                postfix_expr.push(SubExpression::Function(Rc::new(
                                            (args_map,
                                             block.to_vec())
                                )));
                            } else {
                                postfix_expr.push(SubExpression::Name(name.clone()));
                            }
                        }
                        Token::Integer(int) => {
                            postfix_expr.push(SubExpression::Val(Variable::Integer(*int)))
                        }
                        Token::Float(float) => {
                            postfix_expr.push(SubExpression::Val(Variable::Float(*float)))
                        }
                        Token::StringLiteral(string) => {
                            postfix_expr.push(SubExpression::Val(Variable::Str(string.clone())))
                        }
                        _ => panic!(),
                    }
                }

                Token::Operator(token_ty) => {
                    use parser::operator_precedence;
                    let mut last_val = stack.pop();
                    while let Some(Token::Operator(stack_ty)) = last_val {
                        if operator_precedence(stack_ty) > operator_precedence(*token_ty) {
                            break;
                        }
                        postfix_expr.push(SubExpression::Operator(stack_ty));
                        last_val = stack.pop();
                    }
                    if let Some(val) = last_val {
                        stack.push(val);
                    }
                    stack.push(token.clone());
                }

                Token::RParen | Token::EOF | Token::LBrace | Token::Semicolon => {
                    let mut last_val = stack.pop();
                    while !matches!(last_val, Some(Token::LParen) | None) {
                        if let Some(token) = last_val {
                            postfix_expr.push(parser::token_to_subexpr(token));
                            last_val = stack.pop();
                        }
                    }
                    if token != &Token::RParen {
                        break;
                    }
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
                } else if is_ident_char(ch) {
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

    #[allow(clippy::useless_let_if_seq)]
    pub fn read_next_block(
        reader: &mut Option<impl Iterator<Item = Result<String, std::io::Error>> + 'a>,
    ) -> Option<Vec<String>> {
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
                }
                None => {
                    io::stdin().lock().read_line(&mut line).unwrap();
                }
            }

            let mut contains_brace = false;
            if line.contains('{') {
                brace_cnt += 1;
                contains_brace = true;
            }
            if line.contains('}') {
                brace_cnt -= 1;
                let split = line.split('}').collect::<Vec<&str>>();
                ret_block.push(format!("{}{}", split[0], '}'));
                ret_block.push(split[1].to_string());
                if brace_cnt == 0 {
                    ret_block.push(line);
                    break;
                }
                continue;
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

pub fn process_block(in_block: &[&str]) -> Vec<BlockSection> {
    let mut blocks: Vec<BlockSection> = Vec::new();

    let mut line_iterator = in_block.iter().enumerate();
    while let Some((line_num, block_line)) = line_iterator.next() {
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
                let mut final_line: Option<usize> = None;
                let mut next_line = line_iterator.next();
                while let Some((inner_line_num, line)) = next_line {
                    if line.contains('}') {
                        brace_cnt -= 1;
                    }
                    if line.contains('{') {
                        brace_cnt += 1;
                    }
                    if brace_cnt == 0 {
                        final_line = Some(inner_line_num);
                        break;
                    }

                    next_line = line_iterator.next();
                }

                // dbg!(&in_block[line_num + 1 .. final_line.unwrap()]);
                let block = process_block(&in_block[line_num + 1..final_line.unwrap()]);

                blocks.push(BlockSection::InnerBlock(block));

                line = Vec::new();
            }
        }
    }
    // dbg!(blocks.clone());

    blocks
}

pub fn exec_block(
    block: &[BlockSection],
    vars: &mut HashMap<String, Variable>,
    context: &mut InterpreterContext,
) -> Option<Variable> {
    let mut block_iter = block.iter();
    let mut res = None;
    while let Some(block_section) = block_iter.next() {
        match block_section {
            BlockSection::Line(tokens) => {
                let mut token_iter = tokens.iter().peekable();
                while let Some(token) = token_iter.next() {
                    match token {
                        Token::While => {
                            let conditional_expr = Lexer::read_expr(&mut token_iter, &vars);
                            let inner_block = block_iter.next();

                            let mut inner_vars = vars.clone();

                            if let BlockSection::InnerBlock(inner_block_vec) = inner_block.unwrap()
                            {
                                while parser::eval_expr(&conditional_expr, &inner_vars)
                                    == Variable::Bool(true)
                                    {
                                        exec_block(&inner_block_vec, &mut inner_vars, context);
                                    }
                            }

                            for (k, v) in inner_vars {
                                if let Some(old_v) = vars.get_mut(&k) {
                                    *old_v = v;
                                }
                            }
                            break;
                        }

                        Token::Ident(IdentType::If) => {
                            let conditional_expr = Lexer::read_expr(&mut token_iter, &vars);

                            let inner_block = block_iter.next();
                            let mut inner_vars = vars.clone();

                            if let BlockSection::InnerBlock(inner_block_vec) = inner_block.unwrap()
                            {
                                if parser::eval_expr(&conditional_expr, &inner_vars)
                                    == Variable::Bool(true)
                                {
                                    exec_block(&inner_block_vec, &mut inner_vars, context);
                                    context.if_else_status.push(true);
                                } else {
                                    context.if_else_status.push(false);
                                }
                            } else {
                                panic!("Invalid if statement");
                            }
                            for (k, v) in inner_vars {
                                if let Some(old_v) = vars.get_mut(&k) {
                                    *old_v = v;
                                }
                            }

                            break;
                        }

                        Token::Ident(IdentType::Else) => {
                            let prev_status =
                                context.if_else_status.pop().expect("error: invalid else");

                            if !prev_status {
                                let inner_block = block_iter.next();
                                let mut inner_vars = vars.clone();

                                if let BlockSection::InnerBlock(inner_block_vec) =
                                    inner_block.unwrap()
                                {
                                    context.if_else_status.push(true);
                                    exec_block(&inner_block_vec, &mut inner_vars, context);
                                } else {
                                    panic!("Missing else block")
                                }

                                for (k, v) in inner_vars {
                                    if let Some(old_v) = vars.get_mut(&k) {
                                        *old_v = v;
                                    }
                                }

                                break;
                            }
                            block_iter.next();
                        }

                        Token::Ident(ty) => match ty {
                            IdentType::Let => match token_iter.next().unwrap() {
                                Token::Name(lhs) => {
                                    if let Token::Assign = token_iter.next().unwrap() {
                                        let rhs_expr = Lexer::read_expr(&mut token_iter, &vars);
                                        let rhs_eval = parser::eval_expr(&rhs_expr, &vars);
                                        vars.insert(lhs.to_string(), rhs_eval);
                                        break;
                                    } else {
                                        panic!("syntax error after let token")
                                    }
                                }
                                _ => panic!("invalid assign"),
                            },
                            IdentType::Return => {
                                let expr = Lexer::read_expr(&mut token_iter, &vars);
                                res = Some(parser::eval_expr(&expr, &vars));
                                break;
                            }
                            IdentType::Print => {
                                let expr = Lexer::read_expr(&mut token_iter, &vars);
                                println!("{:?}", parser::eval_expr(&expr, &vars));
                                break;
                            }
                            IdentType::Function => {
                                let fn_name =
                                    if let Some(Token::Name(fn_name_rc)) = token_iter.next() {
                                        &*fn_name_rc
                                    } else {
                                        panic!("Invalid function declaration")
                                    };
                                let token_vec = token_iter.filter(|token| token != &&Token::EOF).collect::<Vec<&Token>>();
                                let mut fn_sig_split = token_vec.as_slice().split(|token| token == &&Token::RetArrow);
                                let args_slice = fn_sig_split.next().expect("invalid function signature");

                                let mut fn_args: Vec<(Rc<String>, VarType)> = Vec::new();

                                if let [Token::LParen, args @ .., Token::RParen] = args_slice {
                                    let mut args_iter = args.iter();

                                    while let [&Token::Name(name_rc), &Token::Colon, &Token::Name(type_rc)] =
                                        (&mut args_iter)
                                            .take(3)
                                            .collect::<Vec<&&Token>>()
                                            .as_slice()
                                            {
                                                fn_args.push((name_rc.clone(), type_rc.clone()))
                                            }
                                }


                                let mut ret_type = None;
                                if let Some([Token::Name(ret_type_name_rc)]) = fn_sig_split.next() {
                                    ret_type = Some(ret_type_name_rc.clone());
                                }

                                let block = block_iter.next();
                                if let Some(BlockSection::InnerBlock(block_vec)) = &block {
                                    vars.insert(
                                        fn_name.to_string(),
                                        Variable::Function(Rc::new(SlangFn {
                                            args: fn_args,
                                            block: block_vec.to_vec(),
                                            ret_type,
                                        })),
                                    );
                                } else {
                                    panic!("Invalid function declaration");
                                }

                                break;
                            }
                            _ => todo!(),
                        },

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
                                        let rhs_expr = Lexer::read_expr(&mut token_iter, &vars);
                                        let rhs_eval = parser::eval_expr(&rhs_expr, &vars);

                                        match vars.get_mut(lhs.as_ref()) {
                                            Some(prev_val) => {
                                                if prev_val.same_type(&rhs_eval) {
                                                    *prev_val = rhs_eval;
                                                } else {
                                                    panic!(
                                                        "Variable types do not match: {:?}, {:?}",
                                                        lhs, rhs_eval
                                                    );
                                                }
                                            }
                                            None => panic!(format!(
                                                    "assigned to uninitialized variable {}",
                                                    &lhs
                                            )),
                                        }
                                        break;
                                    } else {
                                        parser::eval_expr(&Lexer::read_expr(&mut [token.clone(), next_token.clone()].iter().chain(token_iter), vars), vars);
                                    }
                                }
                                break;
                            }

                        _ => todo!(),
                    }
                }

                if let Some(token) = tokens.get(0) {
                    if token != &Token::Ident(IdentType::If) {
                        context.if_else_status.pop();
                    }
                }
            }
            BlockSection::InnerBlock(blocks) => {
                exec_block(blocks, vars, context);
            }
        }
        if res != None {
            break;
        }
    }

    res
}

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}
