use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    interpreter::LoxValue,
    lox_error::{Error, LoxError},
    token::Token,
};

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: enclosing,
        }
    }

    pub fn define(&mut self, name: &str, value: &LoxValue) {
        self.values.insert(name.to_owned(), value.clone());
    }

    pub fn assign(&mut self, name: &Token, value: &LoxValue) -> Result<(), LoxError> {
        if self.values.contains_key(name.get_lexeme()) {
            self.values
                .insert(name.get_lexeme().to_owned(), value.clone());
            Ok(())
        } else if let Some(enclosing) = self.enclosing.as_mut() {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(LoxError::RuntimeError(Error::error_with_token(
                name,
                &format!("Undefined variable {}.", name.get_lexeme()),
            )))
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, LoxError> {
        if self.values.contains_key(name.get_lexeme()) {
            Ok(self.values.get(name.get_lexeme()).unwrap().clone())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow().get(name)
        } else {
            let e = LoxError::RuntimeError(Error::error_with_token(
                name,
                &format!("Undefined variable {}.", name.get_lexeme()),
            ));
            Err(e)
        }
    }
}
