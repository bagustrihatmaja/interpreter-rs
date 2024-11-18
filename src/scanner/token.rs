use crate::scanner::token_type::TokenType;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Text(String),
    Double(f64),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    tken_type: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    line: usize,
}

impl Token {
    pub fn new(t: TokenType, lexeme: String, literal: Option<Literal>, line: usize) -> Token {
        Self {
            tken_type: t,
            lexeme: lexeme,
            literal: literal,
            line: line,
        }
    }

    pub fn get_token_type(&self) -> &TokenType {
        &self.tken_type
    }

    pub fn get_lexeme(&self) -> &str {
        &self.lexeme
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Text(s) => write!(f, "{}", s),
            Literal::Double(d) => write!(f, "{}", d),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {:?}", self.tken_type, self.lexeme, self.literal)
    }
}
