use std::fmt;

#[derive(Debug, PartialEq)]
pub enum LoxError {
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

}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let formatted_message  = if self.error_where.is_empty() {
            format!("[line {}] Error: {}", self.line, self.message)
        } else { format!("[line {}] Error {}: {}", self.line, self.error_where, self.message)};
        write!(
            f,
            "{}", formatted_message
        )
    }
}


impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lox_error_message = match self {
            LoxError::LexicalError(error) => format!("{error}"),
            LoxError::UnexpectedError(error) => format!("{error}"),
        };
        write!(f, "{}", lox_error_message)
    }
}
