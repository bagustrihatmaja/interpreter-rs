use core::fmt;

pub enum LoxValue {
    NumberValue(f64),
    Nil,
    BooleanValue(bool),
    StringValue(String),
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxValue::NumberValue(a), LoxValue::NumberValue(b)) => a == b,
            (LoxValue::BooleanValue(a), LoxValue::BooleanValue(b)) => a == b,
            (LoxValue::StringValue(a), LoxValue::StringValue(b)) => a == b,
            _ => false,
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::BooleanValue(a) => write!(f, "{}", a),
            LoxValue::NumberValue(d) => write!(f, "{}", d),
            LoxValue::StringValue(a) => write!(f, "{}", a),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

pub mod interpreter {
    use std::result;

    use crate::{
        expression::{BinaryExpr, Expression, GroupingExpr, Statement, UnaryExpr},
        lox_error::{Error, LoxError},
        token::Literal,
        token_type::TokenType,
    };

    use super::LoxValue;

    pub fn interpret_expression(expression: &Expression) -> Result<LoxValue, LoxError> {
        visit_expression(expression)
    }

    pub fn interpret_statements(statements: &Vec<Statement>) -> i32 {
        let mut error = 0;
        for statement in statements {
            let result = visit_statement(statement);
            match result {
                Err(e) => {
                    e.report();
                    error = 65
                },
                _ => ()
            }
        }
        return error
    }

    fn visit_statement(statement: &Statement) -> Result<LoxValue, LoxError> {
        match statement {
            Statement::PrintStatement(p) => {
                let value = visit_expression(&p.expression)?;
                print!("{}", value);
                Ok(value)
            }
            Statement::ExpressionStatement(e) => visit_expression(&e.expression),
        }
    }

    fn visit_expression(expression: &Expression) -> Result<LoxValue, LoxError> {
        match expression {
            Expression::Literal(e) => e.literal.clone().map_or(Ok(LoxValue::Nil), |f| match f {
                Literal::Text(t) => Ok(LoxValue::StringValue(t)),
                Literal::Double(d) => Ok(LoxValue::NumberValue(d)),
                Literal::Boolean(b) => Ok(LoxValue::BooleanValue(b)),
            }),
            Expression::Binary(binary_expr) => visit_binary(binary_expr),
            Expression::Grouping(grouping_expr) => visit_grouping(grouping_expr),
            Expression::Unary(unary_expr) => visit_unary(unary_expr),
        }
    }

    fn visit_grouping(e: &GroupingExpr) -> Result<LoxValue, LoxError> {
        visit_expression(&e.expression)
    }

    fn visit_unary(expression: &UnaryExpr) -> Result<LoxValue, LoxError> {
        let right = visit_expression(&expression.right)?;
        let operator = &expression.operator;
        let line = operator.get_line();
        match operator.get_token_type() {
            TokenType::Bang => Ok(LoxValue::BooleanValue(!is_truthy(&right))),
            TokenType::Minus => {
                if let LoxValue::NumberValue(n) = right {
                    Ok(LoxValue::NumberValue(n * -1.0))
                } else {
                    Err(LoxError::RuntimeError(Error::error(
                        line,
                        "Operands must be numbers.".into(),
                    )))
                }
            }
            _ => todo!(),
        }
    }

    fn visit_binary(expression: &BinaryExpr) -> Result<LoxValue, LoxError> {
        let left = visit_expression(&expression.left)?;
        let right = visit_expression(&expression.right)?;
        let operator = &expression.operator;
        let line = operator.get_line();

        match operator.get_token_type() {
            TokenType::Greater => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l > r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::GreaterEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l >= r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::Less => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l < r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::LessEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l <= r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::BangEqual => Ok(LoxValue::BooleanValue(!is_equal(&left, &right))),
            TokenType::EqualEqual => Ok(LoxValue::BooleanValue(is_equal(&left, &right))),
            TokenType::Minus => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l - r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::Plus => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l + r))
                }
                (LoxValue::StringValue(l), LoxValue::StringValue(r)) => {
                    Ok(LoxValue::StringValue(String::from(format!("{l}{r}"))))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be two numbers or two strings.".into(),
                ))),
            },
            TokenType::Slash => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l / r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            TokenType::Star => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l * r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.".into(),
                ))),
            },
            _ => Ok(LoxValue::Nil),
        }
    }

    fn is_truthy(object: &LoxValue) -> bool {
        match object {
            LoxValue::BooleanValue(b) => *b,
            LoxValue::StringValue(_) => true,
            LoxValue::NumberValue(_) => true,
            LoxValue::Nil => false,
        }
    }

    fn is_equal(a: &LoxValue, b: &LoxValue) -> bool {
        if *a == LoxValue::Nil && *b == LoxValue::Nil {
            true
        } else if *a == LoxValue::Nil {
            false
        } else {
            a == b
        }
    }
}
