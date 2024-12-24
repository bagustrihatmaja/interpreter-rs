use core::fmt;
use std::fmt::write;

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
    use crate::{
        expression::{BinaryExpr, Expression, GroupingExpr, UnaryExpr},
        token::{Literal, Token},
        token_type::TokenType,
    };

    use super::LoxValue;

    pub fn visit(expression: &Expression) -> LoxValue {
        match expression {
            Expression::Literal(e) => e.literal.clone().map_or(LoxValue::Nil, |f| match f {
                Literal::Text(t) => LoxValue::StringValue(t),
                Literal::Double(d) => LoxValue::NumberValue(d),
            }),
            Expression::Binary(binary_expr) => visit_binary(binary_expr),
            Expression::Grouping(grouping_expr) => visit_grouping(grouping_expr),
            Expression::Unary(unary_expr) => visit_unary(unary_expr),
        }
    }

    fn visit_grouping(e: &GroupingExpr) -> LoxValue {
        visit(&e.expression)
    }

    fn visit_unary(expression: &UnaryExpr) -> LoxValue {
        let right = visit(&expression.right);
        match expression.operator.get_token_type() {
            TokenType::Minus => {
                if let LoxValue::NumberValue(n) = right {
                    LoxValue::NumberValue(n * -1.0)
                } else {
                    todo!()
                }
            }
            TokenType::Bang => LoxValue::BooleanValue(!is_truthy(&right)),
            _ => todo!(),
        }
    }

    fn visit_binary(expression: &BinaryExpr) -> LoxValue {
        let left = visit(&expression.left);
        let right = visit(&expression.right);

        match expression.operator.get_token_type() {
            TokenType::Greater => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::BooleanValue(l > r)
                }
                _ => todo!(),
            },
            TokenType::GreaterEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::BooleanValue(l >= r)
                }
                _ => todo!(),
            },
            TokenType::Less => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::BooleanValue(l < r)
                }
                _ => todo!(),
            },
            TokenType::LessEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::BooleanValue(l <= r)
                }
                _ => todo!(),
            },
            TokenType::BangEqual => LoxValue::BooleanValue(!is_equal(&left, &right)),
            TokenType::EqualEqual => LoxValue::BooleanValue(is_equal(&left, &right)),
            TokenType::Minus => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::NumberValue(l - r)
                }
                _ => todo!(),
            },
            TokenType::Plus => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::NumberValue(l + r)
                }
                (LoxValue::StringValue(l), LoxValue::StringValue(r)) => {
                    LoxValue::StringValue(String::from(format!("{l}{r}")))
                }
                _ => todo!(),
            },
            TokenType::Slash => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::NumberValue(l / r)
                }
                _ => todo!(),
            },
            TokenType::Star => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    LoxValue::NumberValue(l * r)
                }
                _ => todo!(),
            },
            _ => LoxValue::Nil,
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
