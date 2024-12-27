use std::{collections::HashMap, ops::Add};

use crate::{
    interpreter::LoxValue,
    lox_error::{Error, LoxError},
    token::Token,
};

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&self, name: &str, value: &LoxValue) -> Self {
        self.clone()
            + Environment {
                values: HashMap::from([(name.to_string(), value.clone())]),
            }
    }

    pub fn assign(&self, name: &Token, value: &LoxValue) -> Result<Self, LoxError> {
        if self.values.contains_key(name.get_lexeme()) {
            Ok(self.clone()
                + Environment {
                    values: HashMap::from([(name.get_lexeme().into(), value.clone())]),
                })
        } else {
            Err(LoxError::RuntimeError(Error::error_with_token(
                name,
                &format!("Undefined variable {}.", name.get_lexeme()),
            )))
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, LoxError> {
        self.values.get(name.get_lexeme()).map_or(
            Err(LoxError::RuntimeError(Error::error_with_token(
                name,
                &format!("Undefined variable {}.", name.get_lexeme()),
            ))),
            |e| Ok(e.clone()),
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
