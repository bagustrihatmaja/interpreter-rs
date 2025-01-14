use std::{
    cell::RefCell,
    fmt,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    environments::Environment,
    expression::FunctionStmt,
    interpreter::{Interpreter, LoxValue},
    lox_error::LoxError,
};

pub trait LoxCallable: LoxCallableClone {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> LoxValue;
    fn set_arity(&mut self, arity: usize);
    fn get_arity(&self) -> &usize;
    fn to_string(&self) -> String;
}

pub trait LoxCallableClone {
    fn clone_box(&self) -> Box<dyn LoxCallable>;
}

impl<T> LoxCallableClone for T
where
    T: 'static + LoxCallable + Clone,
{
    fn clone_box(&self) -> Box<dyn LoxCallable> {
        Box::new(self.clone())
    }
}

// This is necessary to allow `Box<dyn LoxCallable>` to implement `Clone`.
impl Clone for Box<dyn LoxCallable> {
    fn clone(&self) -> Box<dyn LoxCallable> {
        self.clone_box()
    }
}

pub struct Callable {
    val: Box<dyn LoxCallable>,
}

impl Callable {
    pub fn new(val: Box<dyn LoxCallable>) -> Self {
        Self { val: val }
    }

    pub fn get_arity(&self) -> &usize {
        self.val.get_arity()
    }

    pub fn to_string(&self) -> String {
        String::from(self.val.to_string())
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, LoxError> {
        Ok(self.val.call(interpreter, arguments))
    }
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        Self {
            val: self.val.clone(),
        }
    }
}

#[derive(Clone)]
struct Clock {
    arity: usize,
}

impl Clock {
    pub fn new() -> Self {
        Self { arity: 0 }
    }
}

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<LoxValue>) -> LoxValue {
        let now = SystemTime::now();
        let since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
        LoxValue::NumberValue(since_epoch.as_millis() as f64 / 1000.0)
    }

    fn set_arity(&mut self, arity: usize) {
        self.arity = arity;
    }

    fn get_arity(&self) -> &usize {
        &self.arity
    }
    
    fn to_string(&self) -> String {
        "<fn clock>".into()
    }
}

pub mod primitives {
    use super::{Callable, Clock};

    pub fn clock() -> Callable {
        Callable::new(Box::new(Clock::new()))
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    declaration: FunctionStmt,
    arity: usize,
}

impl LoxFunction {
    pub fn new(declaration: &FunctionStmt) -> Self {
        Self {
            declaration: declaration.clone(),
            arity: declaration.params.len(),
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> LoxValue {
        let mut env = Environment::new(Some(interpreter.get_env().clone()));
        for i in 0..self.declaration.params.len() {
            env.define(
                &self.declaration.params[i].get_lexeme(),
                &arguments[i].clone(),
            );
        }
        let _ = interpreter.execute_block(&self.declaration.body, Rc::new(RefCell::new(env)));
        LoxValue::Nil
    }

    fn set_arity(&mut self, _: usize) {
        self.arity = self.declaration.params.len();
    }

    fn get_arity(&self) -> &usize {
        &self.arity
    }
    
    fn to_string(&self) -> String {
        format!("<fn {}>", self.declaration.name.get_lexeme())
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.declaration.name.get_lexeme())
    }
}
