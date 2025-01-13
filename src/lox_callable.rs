use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    interpreter::{Interpreter, LoxValue},
    lox_error::LoxError,
};

pub trait LoxCallable: LoxCallableClone {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> LoxValue;
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
    arity: u8,
}

impl Callable {
    pub fn new(val: Box<dyn LoxCallable>, arity: u8) -> Self {
        Self {
            val: val,
            arity: arity,
        }
    }

    pub fn get_arity(&self) -> &u8 {
        &self.arity
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
            arity: self.arity,
        }
    }
}


#[derive(Clone)]
struct Clock {}

impl Clock {
    pub fn new() -> Self {
        Self {}
    }
}

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<LoxValue>) -> LoxValue {
        let now = SystemTime::now();
        let since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
        LoxValue::NumberValue(since_epoch.as_millis() as f64 / 1000.0)
    }
}

pub mod primitives {
    use super::{Callable, Clock};

    pub fn clock() -> Callable {
        Callable::new(Box::new(Clock::new()), 0)
    }
}
