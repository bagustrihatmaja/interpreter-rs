use crate::token_type::TokenType;
use std::fmt;

pub type Object = Option<String>;

#[derive(Debug, PartialEq)]
pub struct Token {
    tken_type: TokenType,
    lexeme: String,
    literal: Object,
    line: usize,
}

impl Token {
    pub fn new(t: TokenType, lexeme: String, literal: Object, line: usize) -> Token {
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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {:?}", self.tken_type, self.lexeme, self.literal)
    }
}
