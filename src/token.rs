use crate::token_type::{self, TokenType};
use std::fmt;

type Object = Option<String>;

pub struct Token {
    tken_type: TokenType,
    lexeme: String,
    literal: Object,
    line: i32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {:?}", self.tken_type, self.lexeme, self.literal)
    }
}
