use std::char;

use keywords::RESERVED_KEYWORDS;

use crate::{
    lox_error::{Error, LoxError},
    token::{Literal, Token},
    token_type::TokenType,
};

#[derive(Debug, PartialEq)]
pub struct Scanner<'a> {
    tokens: Vec<Result<Token, LoxError>>,
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Result<Token, LoxError>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Ok(Token::new(
            TokenType::Eof,
            String::from(""),
            None,
            self.line,
        )));
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
                        TokenType::Greater
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
                        self.number();
                    } else if self.is_alpha(ch) {
                        self.identifier();
                    } else {
                        self.tokens.push(Err(LoxError::LexicalError(Error::error(
                            &self.line,
                            &format!("Unexpected character: {}", ch),
                        ))));
                    }
                }
            },
            None => {
                self.tokens.push(Err(LoxError::UnexpectedError(Error::error(
                    &self.line,
                    "Unexpected error, no character to read",
                ))));
            }
        };
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        let token_type = RESERVED_KEYWORDS
            .get(text)
            .cloned()
            .unwrap_or(TokenType::Identifier);
        self.add_token(token_type, None)
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
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
            self.source.chars().nth(self.current + 1).unwrap_or('\0')
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
            self.tokens.push(Err(LoxError::LexicalError(Error::error(
                &self.line,
                "Unterminated string.".into(),
            ))));
            return;
        }

        self.advance();
        let value = self.source.chars().skip(self.start + 1).take(self.current - self.start - 2).collect();
        self.add_token(TokenType::String, Some(Literal::Text(value)));
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
        let ch = self.source.chars().nth(self.current);
        self.current += 1;
        ch
    }

    fn add_token(&mut self, t: TokenType, literal: Option<Literal>) {
        let text = self.source.chars().skip(self.start).take(self.current - self.start).collect();
        let new_token = Token::new(t, text, literal, self.line);
        self.tokens.push(Ok(new_token));
    }
}

mod keywords {
    use crate::token_type::TokenType;
    use phf::phf_map;

    pub const RESERVED_KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While
    };
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
        assert!(scanner.tokens.first().is_some_and(|x| if let Ok(t) = x {
            *t.get_token_type() == TokenType::Eof
        } else {
            false
        }))
    }
}
