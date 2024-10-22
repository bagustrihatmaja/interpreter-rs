use crate::token_type::{self, TokenType};
use std::{fmt, str::FromStr};

pub type Object = Option<String>;

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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {:?}", self.tken_type, self.lexeme, self.literal)
    }
}
