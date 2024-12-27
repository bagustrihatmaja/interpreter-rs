use core::fmt;

use crate::{
    environments::Environment,
    expression::{
        AssignExpr, BinaryExpr, Expression, GroupingExpr, Statement, UnaryExpr, VarExpr,
        VariableExpr,
    },
    lox_error::{Error, LoxError},
    token::Literal,
    token_type::TokenType,
};

#[derive(Clone)]
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

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn interpret_expression(&mut self, expression: &Expression) -> Result<LoxValue, LoxError> {
        self.visit_expression(expression)
    }

    pub fn interpret_statements(&mut self, statements: &Vec<Statement>) -> i32 {
        let mut error = 0;
        for statement in statements {
            let result = self.visit_statement(statement);
            match result {
                Err(e) => {
                    e.report();
                    error = e.get_error_code();
                    break;
                }
                _ => (),
            }
        }
        return error;
    }

    fn visit_statement(&mut self, statement: &Statement) -> Result<LoxValue, LoxError> {
        match statement {
            Statement::PrintStatement(p) => {
                let value = self.visit_expression(&p.expression)?;
                println!("{}", value);
                Ok(value)
            }
            Statement::ExpressionStatement(e) => self.visit_expression(&e.expression),
            Statement::VarStatement(var_expr) => self.visit_var(var_expr),
        }
    }

    fn visit_var(&mut self, statement: &VarExpr) -> Result<LoxValue, LoxError> {
        let lexeme = statement.name.get_lexeme();
        if let Some(e) = &statement.initializer {
            match self.visit_expression(&e) {
                Ok(v) => {
                    self.environment = self.environment.define(lexeme, &v);
                    Ok(v)
                }
                Err(e) => Err(e),
            }
        } else {
            let nil = LoxValue::Nil;
            self.environment = self.environment.define(lexeme, &nil);
            Ok(nil)
        }
    }

    fn visit_expression(&mut self, expression: &Expression) -> Result<LoxValue, LoxError> {
        match expression {
            Expression::Literal(e) => e.literal.clone().map_or(Ok(LoxValue::Nil), |f| match f {
                Literal::Text(t) => Ok(LoxValue::StringValue(t)),
                Literal::Double(d) => Ok(LoxValue::NumberValue(d)),
                Literal::Boolean(b) => Ok(LoxValue::BooleanValue(b)),
            }),
            Expression::Binary(binary_expr) => self.visit_binary(binary_expr),
            Expression::Grouping(grouping_expr) => self.visit_grouping(grouping_expr),
            Expression::Unary(unary_expr) => self.visit_unary(unary_expr),
            Expression::Variable(variable_expr) => self.visit_variable(variable_expr),
            Expression::Assignment(assign_expr) => self.visit_assignment(assign_expr),
        }
    }

    fn visit_assignment(&mut self, expr: &AssignExpr) -> Result<LoxValue, LoxError> {
        let value = self.visit_expression(&expr.value)?;
        self.environment = self.environment.assign(&expr.name, &value)?;
        Ok(value)
    }

    fn visit_variable(&self, expr: &VariableExpr) -> Result<LoxValue, LoxError> {
        self.environment.get(&expr.name)
    }

    fn visit_grouping(&mut self, e: &GroupingExpr) -> Result<LoxValue, LoxError> {
        self.visit_expression(&e.expression)
    }

    fn visit_unary(&mut self, expression: &UnaryExpr) -> Result<LoxValue, LoxError> {
        let right = self.visit_expression(&expression.right)?;
        let operator = &expression.operator;
        let line = operator.get_line();
        match operator.get_token_type() {
            TokenType::Bang => Ok(LoxValue::BooleanValue(!self.is_truthy(&right))),
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

    fn visit_binary(&mut self, expression: &BinaryExpr) -> Result<LoxValue, LoxError> {
        let left = self.visit_expression(&expression.left)?;
        let right = self.visit_expression(&expression.right)?;
        let operator = &expression.operator;
        let line = operator.get_line();

        match operator.get_token_type() {
            TokenType::Greater => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l > r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            TokenType::GreaterEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l >= r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            TokenType::Less => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l < r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            TokenType::LessEqual => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::BooleanValue(l <= r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            TokenType::BangEqual => Ok(LoxValue::BooleanValue(!self.is_equal(&left, &right))),
            TokenType::EqualEqual => Ok(LoxValue::BooleanValue(self.is_equal(&left, &right))),
            TokenType::Minus => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l - r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
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
                    "Operands must be two numbers or two strings.",
                ))),
            },
            TokenType::Slash => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l / r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            TokenType::Star => match (left, right) {
                (LoxValue::NumberValue(l), LoxValue::NumberValue(r)) => {
                    Ok(LoxValue::NumberValue(l * r))
                }
                _ => Err(LoxError::RuntimeError(Error::error(
                    line,
                    "Operands must be numbers.",
                ))),
            },
            _ => Ok(LoxValue::Nil),
        }
    }

    fn is_truthy(&self, object: &LoxValue) -> bool {
        match object {
            LoxValue::BooleanValue(b) => *b,
            LoxValue::StringValue(_) => true,
            LoxValue::NumberValue(_) => true,
            LoxValue::Nil => false,
        }
    }

    fn is_equal(&self, a: &LoxValue, b: &LoxValue) -> bool {
        if *a == LoxValue::Nil && *b == LoxValue::Nil {
            true
        } else if *a == LoxValue::Nil {
            false
        } else {
            a == b
        }
    }
}
