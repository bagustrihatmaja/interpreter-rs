use std::{char, str::Chars};

use crate::{
    token::{Object, Token},
    token_type::TokenType,
};

#[derive(Debug)]
pub struct Scanner {
    tokens: Vec<Token>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
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

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            String::from(""),
            None,
            self.line,
        ));
        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            Some(ch) => match ch {
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                ',' => self.add_token(TokenType::Comma),
                '.' => self.add_token(TokenType::Dot),
                '-' => self.add_token(TokenType::Minus),
                '+' => self.add_token(TokenType::Plus),
                ';' => self.add_token(TokenType::Semicolon),
                '*' => self.add_token(TokenType::Star),
                '!' => {
                    let token = if self.mtch('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    self.add_token(token);
                }
                '=' => {
                    let token = if self.mtch('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    self.add_token(token);
                }
                '<' => {
                    let token = if self.mtch('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    self.add_token(token);
                }
                '>' => {
                    let token = if self.mtch('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Less
                    };
                    self.add_token(token);
                }
                '/' => {
                    if self.mtch('/') {
                        while self.peek() == Some('\n') && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.add_token(TokenType::Slash)
                    }
                }
                ' ' | '\r' | '\t' => (),
                '\n' => self.line += 1,
                _ => (),
            },
            None => (),
        };
    }

    fn mtch(&mut self, expected: char) -> bool {
        let char_at_current = self.source.chars().nth(self.current);
        let current = self.current;
        if self.is_at_end() {
            return false;
        } else if let Some(c) = char_at_current {
            if c != expected {
                return false;
            }
        }
        self.current = current + 1;
        true
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source.chars().nth(self.current)
        }
    }

    fn is_at_end(&self) -> bool {
        (self.current) >= self.source.chars().count()
    }

    fn advance(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn add_token(&mut self, t: TokenType) {
        self.add_token2(t, None);
    }

    fn add_token2(&mut self, t: TokenType, literal: Object) {
        let text: &str = &self.source[self.start..self.current];
        let new_token = Token::new(t, text.to_string(), literal, self.line);
        self.tokens.push(new_token);
    }
}
