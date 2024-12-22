use crate::{
    expression::GroupingExpr,
    lox_error::{Error, LoxError},
    token::{Literal, Token},
    token_type::TokenType,
};

use super::expression::{BinaryExpr, Expression, LiteralExpr, UnaryExpr};

#[derive(Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(self) -> Option<Expression> {
        match self.expression() {
            Ok((_, expr)) => Some(expr),
            Err(e) => {
                e.report();
                None
            }
        }
    }

    fn expression(self) -> Result<(Self, Expression), LoxError> {
        self.equality()
    }

    fn equality(self) -> Result<(Self, Expression), LoxError> {
        let (mut parser, mut expr) = self.comparison()?;
        let types_to_match = [TokenType::BangEqual, TokenType::EqualEqual];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.comparison()?;
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }

        Ok((parser, expr))
    }

    fn comparison(self) -> Result<(Self, Expression), LoxError> {
        let (mut parser, mut expr) = self.term()?;
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
                let (next_parser, right) = parser.term()?;
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }
        Ok((parser, expr))
    }

    fn term(self) -> Result<(Self, Expression), LoxError> {
        let (mut parser, mut expr) = self.factor()?;
        let types_to_match = [TokenType::Minus, TokenType::Plus];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;

            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.factor()?;
                parser = next_parser;
                expr =
                    Expression::Binary(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
            } else {
                break;
            }
        }
        Ok((parser, expr))
    }

    fn factor(self) -> Result<(Self, Expression), LoxError> {
        let (mut parser, mut expr) = self.unary()?;
        let types_to_match = [TokenType::Slash, TokenType::Star];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap().clone();
                let (next_parser, right) = parser.unary()?;
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

        Ok((parser, expr))
    }

    fn unary(self) -> Result<(Self, Expression), LoxError> {
        let types_to_match = [TokenType::Bang, TokenType::Minus];
        let (parser, matched) = self.match_types(&types_to_match);

        if matched {
            let operator = parser.previous().unwrap().clone();
            let (next_parser, right) = parser.unary()?;
            Ok((
                next_parser,
                Expression::Unary(UnaryExpr::new(operator.clone(), Box::new(right))),
            ))
        } else {
            parser.primary()
        }
    }

    fn primary(self) -> Result<(Self, Expression), LoxError> {
        fn match_false(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) = parser.match_types(&[TokenType::False]);
            if matched {
                (
                    next_parser,
                    Some(Expression::Literal(LiteralExpr::new(Some(Literal::Text(
                        "false".to_string(),
                    ))))),
                )
            } else {
                (next_parser, None)
            }
        }

        fn match_true(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) = parser.match_types(&[TokenType::True]);
            if matched {
                (
                    next_parser,
                    Some(Expression::Literal(LiteralExpr::new(Some(Literal::Text(
                        "true".to_string(),
                    ))))),
                )
            } else {
                (next_parser, None)
            }
        }

        fn match_nil(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) = parser.match_types(&[TokenType::Nil]);
            if matched {
                (
                    next_parser,
                    Some(Expression::Literal(LiteralExpr::new(None))),
                )
            } else {
                (next_parser, None)
            }
        }

        fn match_alpha_numeric(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) =
                parser.match_types(&[TokenType::Number, TokenType::String]);
            if matched {
                let literal = next_parser
                    .previous()
                    .map(|f| (f.get_literal().clone()))
                    .flatten();
                (
                    next_parser,
                    Some(Expression::Literal(LiteralExpr::new(literal))),
                )
            } else {
                (next_parser, None)
            }
        }

        fn match_paren(parser: Parser) -> Result<(Parser, Option<Expression>), LoxError> {
            let (next_parser, matched) = parser.match_types(&[TokenType::LeftParen]);
            if matched {
                let (expr_parser, expr) = next_parser.expression()?;
                let (expr_parser, _t) = expr_parser
                    .consume(TokenType::RightParen, "Expect ')' after expression".into())?;
                Ok((
                    expr_parser,
                    Some(Expression::Grouping(GroupingExpr::new(Box::new(expr)))),
                ))
            } else {
                Ok((next_parser, None))
            }
        }

        fn try_match<F>(parser: Parser, matcher: F) -> (Parser, Option<Expression>)
        where
            F: FnOnce(Parser) -> (Parser, Option<Expression>),
        {
            matcher(parser)
        }

        fn process_parser(parser: Parser) -> Result<(Parser, Expression), LoxError> {
            let (next_parser, expression) = try_match(parser, match_false);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = try_match(next_parser, match_true);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = try_match(next_parser, match_nil);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = try_match(next_parser, match_alpha_numeric);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = match_paren(next_parser)?;
            if let Some(expr) = expression {
                Ok((next_parser, expr))
            } else {
                let tk = next_parser.peek().unwrap().clone();
                Err(LoxError::ParseError(
                    next_parser.error(tk, "Expect expression.".into()),
                ))
            }
        }

        process_parser(self)
    }

    fn consume(self, token_type: TokenType, message: String) -> Result<(Self, Token), LoxError> {
        if self.check(token_type) {
            let parser = self.advance();
            let prev = parser.previous().unwrap().clone();
            Ok((parser, prev))
        } else {
            let tk = self.peek().unwrap().clone();
            let parse_error = LoxError::ParseError(self.error(tk, message));
            Err(parse_error)
        }
    }

    fn error(&self, tk: Token, message: String) -> Error {
        Error::error_with_token(tk, message)
    }

    fn synchronize(self) {
        let mut next_parser = self.advance();

        while !next_parser.is_at_end() {
            let aux_parser = next_parser.clone();
            if let Some(t) = aux_parser.previous() {
                if *t.get_token_type() == TokenType::Semicolon {
                    return;
                }
                let peeked = aux_parser.peek();
                if let Some(p) = peeked {
                    match *p.get_token_type() {
                        TokenType::Class
                        | TokenType::Fun
                        | TokenType::Var
                        | TokenType::For
                        | TokenType::If
                        | TokenType::While
                        | TokenType::Print
                        | TokenType::Return => {
                            return;
                        }
                        _ => {}
                    }
                }
                next_parser = aux_parser.advance();
            }
        }
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
        self.tokens.get(self.current)
    }
}
