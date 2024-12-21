use std::fmt;
use std::io::{self, Write};

use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug, PartialEq)]
pub enum LoxError {
    ParseError(Error),
    LexicalError(Error),
    UnexpectedError(Error),
}

#[derive(Debug, PartialEq)]
pub struct Error {
    line: usize,
    error_where: String,
    message: String,
}

impl Error {
    pub fn new(line: usize, error_where: String, message: String) -> Self {
        Self {
            line,
            error_where,
            message,
        }
    }

    pub fn error(line: usize, message: String) -> Self {
        Self {
            line,
            message,
            error_where: "".into(),
        }
    }

    pub fn error_with_token(tk: Token, message: String) -> Self {
        let lox_error = if *(tk.get_token_type()) == TokenType::Eof {
            Error::new(*tk.get_line(), "at end".into(), message)
        } else {
            Error::new(
                *tk.get_line(),
                format!("at '{}'", tk.get_lexeme()).into(),
                message,
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
        };
        write!(f, "{}", lox_error_message)
    }
}
