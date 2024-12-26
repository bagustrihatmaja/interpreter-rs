use std::{collections::HashMap, ops::Add};

use crate::{
    interpreter::LoxValue,
    lox_error::{Error, LoxError},
    token::Token,
};

pub struct Environment {
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    fn define(self, name: &str, value: &LoxValue) -> Self {
        self + Environment {
            values: HashMap::from([(name.to_string(), value.clone())]),
        }
    }

    fn get(&self, name: &Token) -> Result<&LoxValue, LoxError> {
        self.values.get(name.get_lexeme()).map_or(
            Err(LoxError::RuntimeError(Error::error_with_token(
                name.clone(),
                &format!("Undefined variable {}.", name.get_lexeme()),
            ))),
            |e| Ok(e),
        )
    }
}

impl Add for Environment {
    type Output = Environment;

    fn add(self, rhs: Self) -> Self::Output {
        let self_env = self.values;
        let rhs_env = rhs.values;
        Environment {
            values: self_env.into_iter().chain(rhs_env).collect(),
        }
    }
}
