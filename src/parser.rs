use crate::{Variable, OperatorType, Token};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum SubExpression {
    Val(Variable),
    Operator(OperatorType),
    Name(String),
}

pub type Expression = Vec<SubExpression>;

use std::ops;

pub fn eval_expr(expr: Expression, vars: &HashMap<String, Variable>) -> Variable {
    let mut stack = Expression::new();

    expr.iter().for_each(|subexpr|{
        match subexpr {
            SubExpression::Val(v) => stack.push(SubExpression::Val(v.clone())),
            SubExpression::Name(string) => stack.push(SubExpression::Val(vars.get(string).unwrap().clone())),

            SubExpression::Operator(ty) => {
                let val1 = match stack.pop().unwrap(){
                    SubExpression::Val(v) => v,
                    _ => panic!("not a variable"),
                };
                let val2 = match stack.pop().unwrap(){
                    SubExpression::Val(v) => v,
                    _ => panic!("not a variable"),
                };

                let v = match ty {
                    OperatorType::Plus => val2 + val1,
                    OperatorType::Minus => val2 - val1,
                    OperatorType::Multiply => val2 * val1,
                    OperatorType::Divide => val2 / val1,
                    OperatorType::Exponentiate => val2.exp(val1),
                };

                stack.push(SubExpression::Val(v));
            }
        }
    });

    match stack.pop().unwrap() {
        SubExpression::Val(v) => v,
        _ => panic!("expr eval'd to not variable"),
    }
}

impl ops::Add for Variable {
    type Output = Variable;

    fn add(self, rhs: Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 + v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 + v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(v1 as f64 + v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 + v2),
            (Variable::Str(str1), Variable::Str(str2)) => Variable::Str(format!("{}{}", str1, str2)),
            _ => panic!("illegal addition"),
        }
    }
}

impl ops::Sub for Variable {
    type Output = Variable;

    fn sub(self, rhs: Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 - v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 - v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(v1 as f64 - v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 - v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot subtract strings"),
            _ => panic!("illegal subtraction"),
        }
    }
}

impl ops::Mul for Variable {
    type Output = Variable;

    fn mul(self, rhs: Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 * v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 * v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(v1 as f64 * v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 * v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }
}

impl ops::Div for Variable {
    type Output = Variable;

    fn div(self, rhs: Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 / v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 / v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(v1 as f64 / v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 / v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }
}

impl Variable {
    pub fn exp(self, rhs: Variable) -> Variable {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1.pow(v2 as u32)),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1.powi(v2 as i32)),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float((v1 as f64).powf(v2)),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1.powf(v2)),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }
}

pub fn token_to_subexpr(token: Token) -> SubExpression {
    match token {
        Token::Integer(int) => SubExpression::Val(Variable::Integer(int)),
        Token::Float(float) => SubExpression::Val(Variable::Float(float)),
        Token::StringLiteral(string) => SubExpression::Val(Variable::Str(string)),
        Token::Operator(ty) => SubExpression::Operator(ty),
        Token::Name(name) => SubExpression::Name(name),
        _ => panic!("{:?} can't be subexpr", token),
    }
}

pub fn operator_precedence(ty: OperatorType) -> u8 {
    match ty {
        OperatorType::Plus => 1,
        OperatorType::Minus => 1,
        OperatorType::Multiply => 2,
        OperatorType::Divide => 2,
        OperatorType::Exponentiate => 3,
    }
}
