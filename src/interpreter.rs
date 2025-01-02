use core::fmt;
use std::{cell::RefCell, io, rc::Rc};

use crate::{
    environments::Environment,
    expression::{
        AssignExpr, BinaryExpr, BlockStmt, Expression, GroupingExpr, IfStmt, LogicalExpr,
        Statement, UnaryExpr, VarStmt, VariableExpr, WhileStmt,
    },
    lox_error::{Error, LoxError},
    token::Literal,
    token_type::TokenType,
};

#[derive(Clone, Debug)]
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
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new(None))),
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
            Statement::BlockStatement(block_expr) => self.visit_block(block_expr),
            Statement::IfStatement(if_expr) => self.visit_if(if_expr),
            Statement::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt),
        }
    }

    fn visit_if(&mut self, if_expr: &IfStmt) -> Result<LoxValue, LoxError> {
        let v = self.visit_expression(&if_expr.condition)?;
        if self.is_truthy(&v) {
            let _ = self.visit_statement(&if_expr.then_branch)?;
        } else if let Some(ref e) = *if_expr.else_branch {
            let _ = self.visit_statement(e);
        }

        Ok(LoxValue::Nil)
    }

    fn visit_block(&mut self, block_expr: &BlockStmt) -> Result<LoxValue, LoxError> {
        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.environment,
        )))));
        self.execute_block(&block_expr.statements, environment)
    }

    fn execute_block(
        &mut self,
        statements: &Vec<Statement>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, LoxError> {
        let previous = Rc::clone(&self.environment);
        self.environment = environment;
        let mut res: Result<LoxValue, LoxError> = Ok(LoxValue::Nil);

        for statement in statements {
            let result = self.visit_statement(&statement);
            if let Err(e) = result {
                res = Err(e);
            }
        }

        self.environment = previous;
        res
    }

    fn visit_var(&mut self, statement: &VarStmt) -> Result<LoxValue, LoxError> {
        let lexeme = statement.name.get_lexeme();
        if let Some(e) = &statement.initializer {
            match self.visit_expression(&e) {
                Ok(v) => {
                    self.environment.borrow_mut().define(lexeme, &v);
                    Ok(v)
                }
                Err(e) => Err(e),
            }
        } else {
            let nil = LoxValue::Nil;
            self.environment.borrow_mut().define(lexeme, &nil);
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
            Expression::Logical(logical_expr) => self.visit_logical(logical_expr),
        }
    }

    fn visit_logical(&mut self, expr: &LogicalExpr) -> Result<LoxValue, LoxError> {
        let left = self.visit_expression(&expr.left)?;
        if *expr.operator.get_token_type() == TokenType::Or {
            if self.is_truthy(&left) {
                return Ok(left);
            }
        } else {
            if !self.is_truthy(&left) {
                return Ok(left);
            }
        }
        self.visit_expression(&expr.right)
    }

    fn visit_assignment(&mut self, expr: &AssignExpr) -> Result<LoxValue, LoxError> {
        let value = self.visit_expression(&expr.value)?;
        self.environment.borrow_mut().assign(&expr.name, &value)?;
        Ok(value)
    }

    fn visit_variable(&self, expr: &VariableExpr) -> Result<LoxValue, LoxError> {
        self.environment.borrow().get(&expr.name)
    }

    fn visit_grouping(&mut self, e: &GroupingExpr) -> Result<LoxValue, LoxError> {
        self.visit_expression(&e.expression)
    }

    fn visit_while_statement(&mut self, stmt: &WhileStmt) -> Result<LoxValue, LoxError> {
        let mut v = self.visit_expression(&stmt.condition)?;
        while self.is_truthy(&v) {
            let _ = self.visit_statement(&stmt.body)?;
            v = self.visit_expression(&stmt.condition)?;
        }
        Ok(LoxValue::Nil)
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
