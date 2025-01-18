use core::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::{
    environments::Environment,
    expression::{
        AssignExpr, BinaryExpr, BlockStmt, CallExpr, Expression, FunctionStmt, GroupingExpr,
        IfStmt, LogicalExpr, ReturnStmt, Statement, UnaryExpr, VarStmt, VariableExpr, WhileStmt,
    },
    lox_callable::{primitives, Callable, LoxFunction},
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
    LoxCallable(Callable),
    LoxReturn(Box<LoxValue>),
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
            LoxValue::LoxCallable(c) => write!(f, "{}", c.to_string()),
            LoxValue::LoxReturn(lox_value) => write!(f, "{}", *lox_value),
        }
    }
}

#[derive(Clone)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = Environment::new(None);
        env.define("clock", &LoxValue::LoxCallable(primitives::clock()));
        Interpreter {
            environment: Rc::new(RefCell::new(env)),
        }
    }

    pub fn get_env(&self) -> &Rc<RefCell<Environment>> {
        &self.environment
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
                Ok(LoxValue::LoxReturn(_v)) => break,
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
            Statement::FunctionStatement(function_stmt) => self.visit_function(function_stmt),
            Statement::ReturnStatement(return_stmt) => self.visit_return_statement(return_stmt),
        }
    }

    fn visit_return_statement(&mut self, stmt: &ReturnStmt) -> Result<LoxValue, LoxError> {
        let mut value = None;
        if let Some(ref v) = *stmt.value {
            let expr = self.visit_expression(&v)?;
            // println!("{}", expr);
            value = Some(expr);
        }
        Ok(LoxValue::LoxReturn(Box::new(
            value.unwrap_or(LoxValue::Nil),
        )))
    }

    fn visit_function(&mut self, stmt: &FunctionStmt) -> Result<LoxValue, LoxError> {
        let function = LoxValue::LoxCallable(Callable::new(Box::new(LoxFunction::new(stmt))));
        self.environment
            .borrow_mut()
            .define(stmt.name.get_lexeme(), &function);
        Ok(LoxValue::Nil)
    }

    fn visit_if(&mut self, if_expr: &IfStmt) -> Result<LoxValue, LoxError> {
        let v = self.visit_expression(&if_expr.condition)?;
        if self.is_truthy(&v) {
            let maybe_return = self.visit_statement(&if_expr.then_branch)?;
            if let LoxValue::LoxReturn(_) = maybe_return {
                return Ok(maybe_return);
            }
        } else if let Some(ref e) = *if_expr.else_branch {
            let maybe_return = self.visit_statement(e)?;
            if let LoxValue::LoxReturn(_) = maybe_return {
                return Ok(maybe_return);
            }
        }

        Ok(LoxValue::Nil)
    }

    fn visit_block(&mut self, block_expr: &BlockStmt) -> Result<LoxValue, LoxError> {
        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.environment,
        )))));
        self.execute_block(&block_expr.statements, environment)
    }

    pub fn execute_block(
        &mut self,
        statements: &Vec<Statement>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, LoxError> {
        let previous = Rc::clone(&self.environment);
        self.environment = environment;
        let mut res: Result<LoxValue, LoxError> = Ok(LoxValue::Nil);

        for statement in statements {
            let result = self.visit_statement(&statement);
            if let Ok(LoxValue::LoxReturn(_)) = result {
                res = result;
                break;
            }
            if let Err(e) = result {
                res = Err(e);
            }
        }

        self.environment = previous;
        // println!("Execute block: {}", res.clone().unwrap());
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
            Expression::Call(call_expr) => self.visit_call(call_expr),
        }
    }

    fn visit_call(&mut self, call_expr: &CallExpr) -> Result<LoxValue, LoxError> {
        let callee = self.visit_expression(&call_expr.callee)?;
        if let LoxValue::LoxCallable(c) = callee {
            let mut arguments = Vec::new();
            let call_expr_args = &call_expr.arguments;
            for arg in call_expr_args {
                let evaluated_arg = self.visit_expression(&arg)?;
                arguments.push(evaluated_arg);
            }
            if (*c.get_arity() as usize) == arguments.len() {
                c.call(self, arguments)
            } else {
                Err(LoxError::RuntimeError(Error::error_with_token(
                    &call_expr.paren,
                    &format!(
                        "Expected {} arguments but got {}.",
                        c.get_arity(),
                        arguments.len()
                    ),
                )))
            }
        } else {
            Err(LoxError::RuntimeError(Error::error_with_token(
                &call_expr.paren,
                "Can only call functions and classes.",
            )))
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
        let mut left = self.visit_expression(&expression.left)?;
        let mut right = self.visit_expression(&expression.right)?;
        let operator = &expression.operator;
        let line = operator.get_line();

        if let LoxValue::LoxReturn(l) = left {
            left = *l;
        }
        if let LoxValue::LoxReturn(r) = right {
            right = *r;
        }

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
            LoxValue::LoxCallable(_) => true,
            LoxValue::LoxReturn(lox_value) => self.is_truthy(lox_value),
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
