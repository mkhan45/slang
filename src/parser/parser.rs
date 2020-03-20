use crate::expression_eval as expr_ev;
use crate::expression_eval::{Expression, SubExpression};

use crate::{Token, Variable};

use std::collections::HashMap;
use std::io::{self, BufRead};
use std::rc::Rc;

use crate::parser::lexer::Lexer;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum BlockSection {
    Line(Vec<Token>),
    InnerBlock(Vec<BlockSection>),
}

pub fn read_next_expr<'a>(
    token_iter: &mut impl Iterator<Item = &'a Token>,
    vars: &HashMap<String, Variable>,
) -> Expression {
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
                            let mut args_map: HashMap<String, Vec<SubExpression>> =
                                HashMap::with_capacity(args_name_iter.len());

                            if let Some(Token::LParen) = token_iter.next() {
                                for var_name in args_name_iter {
                                    args_map.insert(
                                        var_name.to_string(),
                                        read_next_expr(token_iter, vars),
                                    );
                                }
                            }

                            let block = &slang_fn_rc.as_ref().block;
                            args_map.insert(
                                name.to_string(),
                                vec![SubExpression::Val(Variable::Function(slang_fn_rc.clone()))],
                            );

                            postfix_expr
                                .push(SubExpression::Function(Rc::new((args_map, block.to_vec()))));
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
                        postfix_expr.push(SubExpression::Val(Variable::Str(Rc::new(string.clone().replace("\\t", "\t")))))
                    }
                    _ => panic!(),
                }
            }

            Token::Operator(token_ty) => {
                use expr_ev::operator_precedence;
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
                        postfix_expr.push(expr_ev::token_to_subexpr(token));
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

#[allow(clippy::useless_let_if_seq)]
pub fn read_next_block<'a>(
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

    blocks
}

pub fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}
