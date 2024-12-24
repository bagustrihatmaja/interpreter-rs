use crate::token_type::TokenType;
use std::fmt::{self};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Text(String),
    Double(f64),
}

#[derive(Debug, PartialEq, Clone)]
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

    pub fn get_literal(&self) -> &Option<Literal> {
        &self.literal
    }

    pub fn get_lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn get_line(&self) -> &usize {
        &self.line
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Text(s) => write!(f, "{}", s),
            Literal::Double(d) => {
                if d.fract() == 0.0 {
                    write!(f, "{:.1}", d) // Add `.0` for integers
                } else {
                    write!(f, "{}", d) // Print floating-point numbers as-is
                }
            }
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {:?}", self.tken_type, self.lexeme, self.literal)
    }
}
