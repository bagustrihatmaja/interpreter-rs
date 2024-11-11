use std::{char, fmt, str::Chars};

use crate::{
    token::{Literal, Token},
    token_type::TokenType,
};

#[derive(Debug, PartialEq)]
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
                '(' => self.add_token(TokenType::LeftParen, None),
                ')' => self.add_token(TokenType::RightParen, None),
                '{' => self.add_token(TokenType::LeftBrace, None),
                '}' => self.add_token(TokenType::RightBrace, None),
                ',' => self.add_token(TokenType::Comma, None),
                '.' => self.add_token(TokenType::Dot, None),
                '-' => self.add_token(TokenType::Minus, None),
                '+' => self.add_token(TokenType::Plus, None),
                ';' => self.add_token(TokenType::Semicolon, None),
                '*' => self.add_token(TokenType::Star, None),
                '!' => {
                    let token = if self.mtch('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    self.add_token(token, None);
                }
                '=' => {
                    let token = if self.mtch('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    self.add_token(token, None);
                }
                '<' => {
                    let token = if self.mtch('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    self.add_token(token, None);
                }
                '>' => {
                    let token = if self.mtch('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Less
                    };
                    self.add_token(token, None);
                }
                '/' => {
                    if self.mtch('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.add_token(TokenType::Slash, None)
                    }
                }
                ' ' | '\r' | '\t' => (),
                '\n' => self.line += 1,
                '"' => self.string(),
                _ => {
                    if self.is_digit(ch) {
                        self.number()
                    } else { /* Error */
                    }
                }
            },
            None => (),
        };
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            while self.is_digit(self.peek()) {
                self.advance();
            }
        }
        let parsed_number = &self.source[self.start..self.current].parse::<f64>();

        match parsed_number {
            Ok(d) => self.add_token(TokenType::Number, Some(Literal::Double(*d))),
            Err(_) => panic!("Failed to parse double"),
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.chars().count() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap_or('\0')
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            // Error here
            return;
        }

        self.advance();
        let value = &self.source[(self.start + 1)..(self.current - 1)];
        self.add_token(TokenType::String, Some(Literal::Text(value.into())));
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

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap_or('\0')
        }
    }

    fn is_at_end(&self) -> bool {
        (self.current) >= self.source.chars().count()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.chars().nth(self.current + 1);
        self.current += 1;
        ch
    }

    fn add_token(&mut self, t: TokenType, literal: Option<Literal>) {
        let text: &str = &self.source[self.start..self.current];
        let new_token = Token::new(t, text.to_string(), literal, self.line);
        self.tokens.push(new_token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_scanner() {
        let scanner = Scanner::new("// this is a test");
        assert_eq!(
            scanner,
            Scanner {
                source: "// this is a test".into(),
                tokens: Vec::new(),
                start: 0,
                current: 0,
                line: 1
            }
        )
    }

    #[test]
    fn parse_and_ignore_comments() {
        let comment = "// th";
        let mut scanner = Scanner::new(comment.into());
        scanner.scan_tokens();
        assert_eq!(scanner.current, comment.chars().count());
        assert_eq!(scanner.tokens.len(), 1);
        assert!(scanner
            .tokens
            .first()
            .is_some_and(|x| *x.get_token_type() == TokenType::Eof))
    }
}
