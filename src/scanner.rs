use crate::token::Token;

pub struct Scanner {
    tokens: Vec<Token>,
    source: String,
    start: i32,
    current: i32,
    line: i32,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Self {
            source: source.to_string(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens() -> Vec<Token> {
        todo!()
    }

    fn is_at_end(&self) -> bool {
        (self.current as usize) >= self.source.chars().count()
    }
}
