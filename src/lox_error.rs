use std::fmt;
use std::io::{self, Write};

use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug, PartialEq)]
pub enum LoxError {
    ParseError(Error),
    LexicalError(Error),
    UnexpectedError(Error),
    RuntimeError(Error),
}

#[derive(Debug, PartialEq)]
pub struct Error {
    line: usize,
    error_where: String,
    message: String,
}

impl Error {
    pub fn new(line: &usize, error_where: String, message: String) -> Self {
        Self {
            line: *line,
            error_where,
            message,
        }
    }

    pub fn error(line: &usize, message: &str) -> Self {
        Self {
            line: *line,
            message: message.into(),
            error_where: "".into(),
        }
    }

    pub fn error_with_token(tk: Token, message: &str) -> Self {
        let lox_error = if *(tk.get_token_type()) == TokenType::Eof {
            Error::new(tk.get_line(), "at end".into(), message.into())
        } else {
            Error::new(
                tk.get_line(),
                format!("at '{}'", tk.get_lexeme()).into(),
                message.into(),
            )
        };
        lox_error
    }

    pub fn report(&self) {
        writeln!(io::stderr(), "{}", self).unwrap();
    }
}

impl LoxError {
    pub fn report(&self) {
        match self {
            LoxError::LexicalError(error) => error.report(),
            LoxError::UnexpectedError(error) => error.report(),
            LoxError::ParseError(error) => error.report(),
            LoxError::RuntimeError(error) => error.report(),
        }
    }

    pub fn get_error_code(&self) -> i32 {
        match self {
            LoxError::LexicalError(error) => 65,
            LoxError::UnexpectedError(error) => 65,
            LoxError::ParseError(error) => 65,
            LoxError::RuntimeError(error) => 70,
        }

    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let formatted_message = if self.error_where.is_empty() {
            format!("[line {}] Error: {}", self.line, self.message)
        } else {
            format!(
                "[line {}] Error {}: {}",
                self.line, self.error_where, self.message
            )
        };
        write!(f, "{}", formatted_message)
    }
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lox_error_message = match self {
            LoxError::LexicalError(error) => format!("{error}"),
            LoxError::UnexpectedError(error) => format!("{error}"),
            LoxError::ParseError(error) => format!("{error}"),
            LoxError::RuntimeError(error) => format!("{}\n[line {}]", error.message, error.line),
        };
        write!(f, "{}", lox_error_message)
    }
}
