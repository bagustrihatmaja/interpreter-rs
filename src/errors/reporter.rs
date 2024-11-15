use std::fmt;

pub mod report {
    use super::ErrorType;

    pub fn error(error_type: ErrorType, m: &str, line: &u64) {
        panic!("{}: {}:L{}", error_type, m, line)
    }
}

#[derive(Debug)]
pub enum ErrorType {
    UnexpectedCharacter,
    UnterminatedString,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error_type = match self {
            ErrorType::UnexpectedCharacter => "UnexpectedCharacter",
            ErrorType::UnterminatedString => "UnterminatedString",
            _ => "UnknownError",
        };
        write!(f, "{}", error_type)
    }
}
