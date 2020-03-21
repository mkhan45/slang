use std::collections::HashMap;

use std::rc::Rc;

pub mod parser;
use parser::{read_next_expr, BlockSection};

mod expression_eval;
use crate::expression_eval as expr_ev;

pub mod tests;

use std::iter;

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

#[derive(Clone, Debug, PartialEq)]
pub struct InterpreterContext {
    if_else_status: Vec<bool>,
    ret_val: Option<Variable>,
    should_break: bool,
    line_num: usize,
}

impl Default for InterpreterContext {
    fn default() -> Self {
        InterpreterContext {
            if_else_status: Vec::with_capacity(1),
            ret_val: None,
            should_break: false,
            line_num: 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct SlangFn {
    args: Vec<(Rc<String>, VarType)>,
    block: Vec<BlockSection>,
    ret_type: Option<VarType>,
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

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Null => write!(f, "NULL"),
            Variable::Integer(v) => write!(f, "{}", v),
            Variable::Float(v) => write!(f, "{}", v),
            Variable::Str(str_rc) => write!(f, "{}", str_rc.as_ref()),
            Variable::Bool(v) => write!(f, "{}", v),
            Variable::Function(_) => panic!("cannot print a function"),
            Variable::Type(type_rc) => write!(f, "Type: {}", type_rc.as_ref()),
            Variable::Custom(_) => unimplemented!(),
        }
    }
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
    Elif,
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

pub fn exec_block(
    block: &[BlockSection],
    vars: &mut HashMap<String, Variable>,
    context: &mut InterpreterContext,
) {
    let mut block_iter = block.iter();
    while let Some(block_section) = block_iter.next() {
        if context.should_break {
            break;
        }
        match block_section {
            BlockSection::Line(tokens) => {
                context.line_num += 1;
                let mut token_iter = tokens.iter().peekable();
                while let Some(token) = token_iter.next() {
                    match token {
                        Token::While => {
                            process_while_loop(&mut block_iter, &mut token_iter, vars, context);
                            break;
                        }

                        Token::Ident(IdentType::If) => {
                            process_if_statement(&mut block_iter, &mut token_iter, vars, context);
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
                                    panic!("Error on line {}, Missing block for else statement", context.line_num)
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

                        Token::Ident(IdentType::Elif) => {
                            let prev_status =
                                context.if_else_status.pop().expect(&format!("Error on line {}: invalid else", context.line_num));

                            if !prev_status {
                                process_if_statement(&mut block_iter, &mut token_iter, vars, context);
                            }
                            block_iter.next();
                        }

                        Token::Ident(ty) => match ty {
                            IdentType::Let => match token_iter.next().unwrap() {
                                Token::Name(lhs) => {
                                    if let Token::Assign = token_iter.next().unwrap() {
                                        let rhs_expr = read_next_expr(&mut token_iter, &vars);
                                        let rhs_eval = expr_ev::eval_expr(&rhs_expr, &vars);
                                        vars.insert(lhs.to_string(), rhs_eval);
                                        break;
                                    } else {
                                        panic!("Error on line {}: syntax error after let token", context.line_num)
                                    }
                                }
                                _ => panic!("Error on line {}: invalid assign", context.line_num),
                            },
                            IdentType::Return => {
                                let expr = read_next_expr(&mut token_iter, &vars);
                                context.ret_val = Some(expr_ev::eval_expr(&expr, &vars));
                                context.should_break = true;
                                break;
                            }
                            IdentType::Print => {
                                let expr = read_next_expr(&mut token_iter, &vars);
                                println!("{}", expr_ev::eval_expr(&expr, &vars));
                                break;
                            }
                            IdentType::Function => {
                                process_function_declaration(
                                    &mut block_iter,
                                    &mut token_iter,
                                    vars,
                                    context,
                                );
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
                                    let rhs_expr = read_next_expr(&mut token_iter, &vars);
                                    let rhs_eval = expr_ev::eval_expr(&rhs_expr, &vars);

                                    match vars.get_mut(lhs.as_ref()) {
                                        Some(prev_val) => {
                                            if prev_val.same_type(&rhs_eval) {
                                                *prev_val = rhs_eval;
                                            } else {
                                                panic!(
                                                    "Error on line {}: Variable types do not match: {:?}, {:?}",
                                                    context.line_num, lhs, rhs_eval
                                                );
                                            }
                                        }
                                        None => panic!(format!(
                                            "Error on line {}: assigned to uninitialized variable {}",
                                            context.line_num,
                                            &lhs
                                        )),
                                    }
                                    break;
                                } else {
                                    expr_ev::eval_expr(
                                        &read_next_expr(
                                            &mut [token.clone(), next_token.clone()]
                                                .iter()
                                                .chain(token_iter),
                                            vars,
                                        ),
                                        vars,
                                    );
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
    }
}

type TokenIterPeekable<'a> = iter::Peekable<std::slice::Iter<'a, Token>>;
type BlockIter<'a> = std::slice::Iter<'a, parser::BlockSection>;

fn process_while_loop(
    block_iter: &mut BlockIter,
    token_iter: &mut TokenIterPeekable,
    vars: &mut HashMap<String, Variable>,
    context: &mut InterpreterContext,
) {
    let conditional_expr = read_next_expr(token_iter, &vars);
    let inner_block = block_iter.next();

    let mut inner_vars = vars.clone();

    if let BlockSection::InnerBlock(inner_block_vec) = inner_block.unwrap() {
        while expr_ev::eval_expr(&conditional_expr, &inner_vars) == Variable::Bool(true) {
            exec_block(&inner_block_vec, &mut inner_vars, context);
            if context.should_break {
                break;
            }
        }
    }

    for (k, v) in inner_vars {
        if let Some(old_v) = vars.get_mut(&k) {
            *old_v = v;
        }
    }
}

fn process_if_statement(
    block_iter: &mut BlockIter,
    token_iter: &mut TokenIterPeekable,
    vars: &mut HashMap<String, Variable>,
    context: &mut InterpreterContext,
) {
    let conditional_expr = read_next_expr(token_iter, &vars);

    let inner_block = block_iter.next();
    let mut inner_vars = vars.clone();

    if let BlockSection::InnerBlock(inner_block_vec) = inner_block.unwrap() {
        if expr_ev::eval_expr(&conditional_expr, &inner_vars) == Variable::Bool(true) {
            exec_block(&inner_block_vec, &mut inner_vars, context);
            context.if_else_status.push(true);
        } else {
            context.if_else_status.push(false);
        }
    } else {
        panic!("Error on line {}: Invalid if statement", context.line_num);
    }
    for (k, v) in inner_vars {
        if let Some(old_v) = vars.get_mut(&k) {
            *old_v = v;
        }
    }
}

fn process_function_declaration(
    block_iter: &mut BlockIter,
    token_iter: &mut TokenIterPeekable,
    vars: &mut HashMap<String, Variable>,
    context: &InterpreterContext,
) {
    let fn_name = if let Some(Token::Name(fn_name_rc)) = token_iter.next() {
        &*fn_name_rc
    } else {
        panic!("Error on line {}: Invalid function declaration", context.line_num)
    };
    let token_vec = token_iter
        .filter(|token| token != &&Token::EOF)
        .collect::<Vec<&Token>>();
    let mut fn_sig_split = token_vec
        .as_slice()
        .split(|token| token == &&Token::RetArrow);
    let args_slice = fn_sig_split.next().expect("invalid function signature");

    let mut fn_args: Vec<(Rc<String>, VarType)> = Vec::new();

    if let [Token::LParen, args @ .., Token::RParen] = args_slice {
        let mut args_iter = args.iter();

        while let [&Token::Name(name_rc), &Token::Colon, &Token::Name(type_rc)] = (&mut args_iter)
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
        panic!("Error on line {}: Invalid function declaration", context.line_num);
    }
}
