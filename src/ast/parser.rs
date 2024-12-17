use crate::scanner::{
    token::{Literal, Token},
    token_type::TokenType,
};
use if_chain::if_chain;

use super::expression::{BinaryExpr, Expression, LiteralExpr, UnaryExpr};

#[derive(Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: u64,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    fn expression(self) -> (Self, Expression) {
        self.equality()
    }

    fn equality(self) -> (Self, Expression) {
        let (mut parser, mut expr) = self.comparison();
        let types_to_match = [TokenType::BangEqual, TokenType::EqualEqual];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.comparison();
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }

        (parser, expr)
    }

    fn comparison(self) -> (Self, Expression) {
        let (mut parser, mut expr) = self.term();
        let types_to_match = [
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.term();
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }
        (parser, expr)
    }

    fn term(self) -> (Self, Expression) {
        let (mut parser, mut expr) = self.factor();
        let types_to_match = [TokenType::Minus, TokenType::Plus];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;

            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.factor();
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }
        (parser, expr)
    }

    fn factor(self) -> (Self, Expression) {
        let (mut parser, mut expr) = self.unary();
        let types_to_match = [TokenType::Slash, TokenType::Star];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.unary();
                parser = next_parser;
                expr = Expression::Binary(BinaryExpr::new(
                    Box::new(expr),
                    operator.clone(),
                    Box::new(right),
                ));
            } else {
                break;
            }
        }

        (parser, expr)
    }

    fn unary(self) -> (Self, Expression) {
        let types_to_match = [TokenType::Bang, TokenType::Minus];
        let (parser, matched) = self.match_types(&types_to_match);

        if matched {
            let operator = parser.previous().unwrap().clone();
            let (next_parser, right) = parser.unary();
            (
                next_parser,
                Expression::Unary(UnaryExpr::new(operator.clone(), Box::new(right))),
            )
        } else {
            parser.primary()
        }
    }

    fn primary(self) -> (Self, Expression) {
        let mut primary_expr = None;
        let mut parser = self;
        let (next_parser, matched) = parser.match_types(&[TokenType::False]);
        parser = next_parser;
        if matched {
            return (
                parser.clone(),
                Expression::Literal(LiteralExpr::new(Some(Literal::Text("false".to_string())))),
            );
        }

        let (next_parser, matched) = parser.match_types(&[TokenType::True]);
        parser = next_parser;
        if matched {
            return (
                parser.clone(),
                Expression::Literal(LiteralExpr::new(Some(Literal::Text("false".to_string())))),
            );
        }

        let (next_parser, matched) = parser.match_types(&[TokenType::Nil]);
        parser = next_parser;
        if matched {
            return (parser.clone(), Expression::Literal(LiteralExpr::new(None)));
        }

        let (next_parser, matched) = parser.match_types(&[TokenType::Number, TokenType::String]);
        parser = next_parser;
        if matched {
            return (parser.clone(), Expression::Literal(LiteralExpr::new(parser.previous().map(|f| *f.get_literal()).flatten())));
        }

        primary_expr.unwrap()
    }

    fn match_types(self, types: &[TokenType]) -> (Self, bool) {
        if types.iter().any(|&t| self.check(t)) {
            let parser = self.advance();
            (parser, true)
        } else {
            (self, false)
        }
    }

    fn advance(self) -> Self {
        if !self.is_at_end() {
            Self {
                tokens: self.tokens,
                current: self.current + 1,
            }
        } else {
            self
        }
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get((self.current - 1) as usize)
    }

    fn check(&self, t: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            let peek_result = self.peek();
            peek_result.is_some_and(|x| *x.get_token_type() == t)
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .is_some_and(|x| *x.get_token_type() == TokenType::Eof)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get((self.current - 1) as usize)
    }
}
