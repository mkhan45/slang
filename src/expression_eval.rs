use crate::{exec_block, BlockSection, InterpreterContext, OperatorType, Token, Variable};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SubExpression {
    Val(Variable),
    Function(Rc<(HashMap<String, Expression>, Vec<BlockSection>)>),
    Operator(OperatorType),
    Name(Rc<String>),
}

pub type Expression = Vec<SubExpression>;

use std::ops;

pub fn eval_expr(expr: &[SubExpression], vars: &HashMap<String, Variable>) -> Variable {
    let mut stack: Vec<SubExpression> = Vec::with_capacity(5);

    expr.iter().for_each(|subexpr| match subexpr {
        SubExpression::Val(_) => stack.push(subexpr.clone()),
        SubExpression::Name(string) => stack.push(SubExpression::Val(
            vars.get(string.as_ref())
                .unwrap_or_else(|| panic!("Uninitialized variable: {}", string))
                .clone(),
        )),
        SubExpression::Function(slang_fn_rc) => {
            let (unprocessed_fn_vars, block) = slang_fn_rc.as_ref();

            let mut fn_vars: HashMap<String, Variable> = unprocessed_fn_vars
                .iter()
                .map(|(k, v)| (k.clone(), eval_expr(v, vars)))
                .collect();

            if let Some(res) = exec_block(block, &mut fn_vars, &mut InterpreterContext::default()) {
                stack.push(SubExpression::Val(res));
            } else {
                stack.push(SubExpression::Val(Variable::Null));
            }
        }
        SubExpression::Operator(ty) => {
            let val1 = match stack.pop() {
                Some(SubExpression::Val(v)) => v,
                _ => panic!("not a variable"),
            };
            let val2 = match stack.pop() {
                Some(SubExpression::Val(v)) => v,
                _ => panic!("not a variable"),
            };

            let v = match ty {
                OperatorType::Cast => val2.cast(&val1),
                OperatorType::Plus => &val2 + &val1,
                OperatorType::Minus => &val2 - &val1,
                OperatorType::Multiply => &val2 * &val1,
                OperatorType::Divide => &val2 / &val1,
                OperatorType::Exponentiate => val2.exp(&val1),
                OperatorType::GreaterEqual => Variable::Bool(val2 >= val1),
                OperatorType::LessEqual => Variable::Bool(val2 <= val1),
                OperatorType::Greater => Variable::Bool(val2 > val1),
                OperatorType::Less => Variable::Bool(val2 < val1),
                OperatorType::NotEqual => Variable::Bool(val2 != val1),
                OperatorType::Equal => Variable::Bool(val2 == val1),
                OperatorType::And => val2.and(&val1),
                OperatorType::Or => val2.or(&val1),
                OperatorType::Modulus => val2.modulus(&val1),
            };

            stack.push(SubExpression::Val(v));
        }
    });

    match stack.pop() {
        Some(SubExpression::Val(v)) => v,
        _ => panic!(format!("expr eval'd to not variable: {:?}", expr)),
    }
}

use std::cmp::Ordering;
impl PartialOrd for Variable {
    #[inline]
    fn partial_cmp(&self, rhs: &Variable) -> Option<Ordering> {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => v1.partial_cmp(v2),
            (Variable::Float(v1), Variable::Integer(v2)) => v1.partial_cmp(&(*v2 as f64)),
            (Variable::Integer(v1), Variable::Float(v2)) => (*v1 as f64).partial_cmp(v2),
            (Variable::Float(v1), Variable::Float(v2)) => v1.partial_cmp(v2),
            _ => None,
        }
    }
}

impl ops::Add for &Variable {
    type Output = Variable;

    #[inline]
    fn add(self, rhs: &Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 + v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 + *v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(*v1 as f64 + v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 + v2),
            (Variable::Str(v1), Variable::Str(v2)) => {
                Variable::Str(Rc::new(format!("{}{}", v1, v2)))
            }
            _ => panic!("illegal addition"),
        }
    }
}

impl ops::Sub for &Variable {
    type Output = Variable;

    #[inline]
    fn sub(self, rhs: &Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 - v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 - *v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(*v1 as f64 - v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 - v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot subtract strings"),
            _ => panic!(format!("illegal subtraction: {:?} - {:?}", self, rhs)),
        }
    }
}

impl ops::Mul for &Variable {
    type Output = Variable;

    #[inline]
    fn mul(self, rhs: &Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 * v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 * *v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(*v1 as f64 * v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 * v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }
}

impl ops::Div for &Variable {
    type Output = Variable;

    #[inline]
    fn div(self, rhs: &Variable) -> Self::Output {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1 / v2),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1 / *v2 as f64),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float(*v1 as f64 / v2),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1 / v2),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }
}

impl Variable {
    #[inline]
    pub fn exp(self, rhs: &Variable) -> Variable {
        match (self, rhs) {
            (Variable::Integer(v1), Variable::Integer(v2)) => Variable::Integer(v1.pow(*v2 as u32)),
            (Variable::Float(v1), Variable::Integer(v2)) => Variable::Float(v1.powi(*v2 as i32)),
            (Variable::Integer(v1), Variable::Float(v2)) => Variable::Float((v1 as f64).powf(*v2)),
            (Variable::Float(v1), Variable::Float(v2)) => Variable::Float(v1.powf(*v2)),
            (Variable::Str(_), Variable::Str(_)) => panic!("cannot multiply strings"),
            _ => panic!("illegal multiplication"),
        }
    }

    #[inline]
    pub fn cast(&self, rhs: &Variable) -> Variable {
        if let Variable::Type(typename) = rhs {
            match (self, typename.as_str()) {
                (Variable::Integer(v1), "String") => Variable::Str(Rc::new(v1.to_string())),
                (Variable::Bool(v1), "String") => Variable::Str(Rc::new(v1.to_string())),
                _ => todo!(),
            }
        } else {
            panic!("Illegal cast to non type variable");
        }
    }

    #[inline]
    pub fn and(&self, rhs: &Variable) -> Variable {
        if let (Variable::Bool(bool1), Variable::Bool(bool2)) = (self, rhs) {
            Variable::Bool(*bool1 && *bool2)
        } else {
            panic!(format!(
                "Illegal And use with non bool: {:?}, {:?}",
                self, rhs
            ));
        }
    }

    #[inline]
    pub fn or(&self, rhs: &Variable) -> Variable {
        if let (Variable::Bool(bool1), Variable::Bool(bool2)) = (self, rhs) {
            Variable::Bool(*bool1 || *bool2)
        } else {
            panic!("Illegal cast to non type variable");
        }
    }

    #[inline]
    pub fn modulus(&self, rhs: &Variable) -> Variable {
        if let (Variable::Integer(v1), Variable::Integer(v2)) = (self, rhs) {
            Variable::Integer(v1 % v2)
        } else {
            panic!("Illegal cast to non type variable");
        }
    }
}

#[inline]
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
        OperatorType::And => 5,
        OperatorType::Or => 5,
        OperatorType::Plus => 4,
        OperatorType::Minus => 4,
        OperatorType::Multiply => 3,
        OperatorType::Divide => 3,
        OperatorType::Exponentiate => 2,
        OperatorType::Equal => 1,
        OperatorType::NotEqual => 1,
        OperatorType::LessEqual => 1,
        OperatorType::GreaterEqual => 1,
        OperatorType::Less => 1,
        OperatorType::Greater => 1,
        OperatorType::Cast => 0,
        OperatorType::Modulus => 1,
    }
}
