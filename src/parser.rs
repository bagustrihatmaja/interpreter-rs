use crate::{
    expression::{
        AssignExpr, BlockStmt, CallExpr, ExpressionStmt, FunctionStmt, GroupingExpr, IfStmt,
        LogicalExpr, PrintStmt, ReturnStmt, Statement, VarStmt, VariableExpr, WhileStmt,
    },
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

type ParsedStatement<'a> = (Parser<'a>, Statement);
type ParseErrorT<'a> = (Parser<'a>, LoxError);
type ParsedStatementOrError<'a> = Result<ParsedStatement<'a>, ParseErrorT<'a>>;
type ParsedExpression<'a> = (Parser<'a>, Expression);
type ParsedExpressionOrError<'a> = Result<ParsedExpression<'a>, ParseErrorT<'a>>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse_expression(self) -> Option<Expression> {
        match self.expression() {
            Ok((_, expr)) => Some(expr),
            Err((_, p)) => {
                p.report();
                None
            }
        }
    }

    pub fn parse(self) -> Vec<Result<Statement, LoxError>> {
        let mut statements = Vec::new();
        let mut parser = self;

        while !parser.is_at_end() {
            let result = parser.declaration();
            match result {
                Ok((next_parser, s)) => {
                    parser = next_parser;
                    statements.push(Ok(s));
                }
                Err((p, e)) => {
                    e.report();
                    parser = p;
                    statements.push(Err(e));
                }
            }
        }

        statements
    }

    fn declaration(self) -> ParsedStatementOrError<'a> {
        let types_to_match = [TokenType::Fun];
        let (parser, matched) = self.match_types(&types_to_match);
        if matched {
            return parser.function("function");
        }

        let types_to_match = [TokenType::Var];
        let (parser, matched) = parser.match_types(&types_to_match);
        let result_or_error = if matched {
            parser.var_declaration()
        } else {
            parser.statements()
        };

        match result_or_error {
            Ok((parser, statement)) => return Ok((parser, statement)),
            Err((parser, e)) => {
                let p = parser.synchronize();
                return Err((p, e));
            }
        }
    }

    fn function(self, kind: &str) -> ParsedStatementOrError<'a> {
        let (parser, name) =
            self.consume(TokenType::Identifier, &format!("Expect {} name.", kind))?;
        let (mut parser, _) = parser.consume(
            TokenType::LeftParen,
            &format!("Expect '(' after  {} name.", kind),
        )?;
        let mut params: Vec<Token> = Vec::new();
        if !parser.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let p = parser.clone();
                    return Err((
                        p,
                        LoxError::ParseError(parser.error(
                            &parser.peek().unwrap(),
                            "Can't have more than 255 parameters.",
                        )),
                    ));
                }
                let (next_parser, t) =
                    parser.consume(TokenType::Identifier, "Expect parameter name.")?;
                parser = next_parser;
                params.push(t);
                let (next_parser, matched) = parser.match_types(&[TokenType::Comma]);
                parser = next_parser;
                if !matched {
                    break;
                }
            }
        }
        let (parser, _) = parser.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        let (parser, _) = parser.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        let (parser, body) = parser.block()?;
        Ok((
            parser,
            Statement::FunctionStatement(FunctionStmt::new(name, params, body)),
        ))
    }

    fn var_declaration(self) -> ParsedStatementOrError<'a> {
        let (parser, name) = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let types_to_match = [TokenType::Equal];
        let (mut parser, matched) = parser.match_types(&types_to_match);

        let expression = if matched {
            let (next_parser, expr) = parser.expression()?;
            parser = next_parser;
            Some(expr)
        } else {
            None
        };

        let (parser, _) = parser.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok((
            parser,
            Statement::VarStatement(VarStmt::new(name, expression)),
        ))
    }

    fn statements(self) -> ParsedStatementOrError<'a> {
        let (parser, matched) = self.match_types(&[TokenType::For]);
        if matched {
            return parser.for_statement();
        }

        let (parser, matched) = parser.match_types(&[TokenType::If]);
        if matched {
            return parser.if_statement();
        }

        let (parser, matched) = parser.match_types(&[TokenType::Print]);
        if matched {
            return parser.print_statement();
        }

        let (parser, matched) = parser.match_types(&[TokenType::Return]);
        if matched {
            return parser.return_statement();
        }

        let (parser, matched) = parser.match_types(&[TokenType::While]);
        if matched {
            return parser.while_statement();
        }

        let (parser, matched) = parser.match_types(&[TokenType::LeftBrace]);
        if matched {
            let maybe_statements = parser.block();
            match maybe_statements {
                Ok((next_parser, statements)) => {
                    return Ok((
                        next_parser,
                        Statement::BlockStatement(BlockStmt::new(statements)),
                    ))
                }
                Err((next_parser, error)) => return Err((next_parser, error)),
            }
        }

        parser.expression_statement()
    }

    fn return_statement(self) -> ParsedStatementOrError<'a> {
        let keyword = self.previous().unwrap();
        let mut value: Option<Expression> = None;
        let mut parser = self;

        if !parser.check(TokenType::Semicolon) {
            let (next_parser, expr) = parser.expression()?;
            parser = next_parser;
            value = Some(expr);
        }

        let (parser, _) = parser.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok((
            parser,
            Statement::ReturnStatement(ReturnStmt::new(keyword, Box::new(value))),
        ))
    }

    fn for_statement(self) -> ParsedStatementOrError<'a> {
        let (parser, _) = self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let mut initializer: Option<Statement> = None;
        let (mut parser, matched_semicolon) = parser.match_types(&[TokenType::Semicolon]);
        if matched_semicolon {
        }
        // do nothing
        else if initializer.is_none() {
            let (next_parser, matched_var) = parser.match_types(&[TokenType::Var]);
            parser = next_parser;
            if matched_var {
                let (next_parser, stmt) = parser.var_declaration()?;
                parser = next_parser;
                initializer = Some(stmt);
            } else {
                let (next_parser, stmt) = parser.expression_statement()?;
                parser = next_parser;
                initializer = Some(stmt);
            }
        }

        let mut condition: Option<Expression> = None;
        if !parser.check(TokenType::Semicolon) {
            let (next_parser, c) = parser.expression()?;
            parser = next_parser;
            condition = Some(c);
        }
        let (mut parser, _) =
            parser.consume(TokenType::Semicolon, "Expect ';' after a loop condition.")?;

        let mut increment: Option<Expression> = None;
        if !parser.check(TokenType::RightParen) {
            let (next_parser, c) = parser.expression()?;
            parser = next_parser;
            increment = Some(c);
        }
        let (parser, _) = parser.consume(TokenType::RightParen, "Expect ')' after for clause.")?;
        let (parser, mut body) = parser.statements()?;

        if let Some(i) = increment {
            let statements = vec![
                body,
                Statement::ExpressionStatement(ExpressionStmt::new(Box::new(i))),
            ];
            body = Statement::BlockStatement(BlockStmt::new(statements));
        }

        let condition = condition.unwrap_or(Expression::Literal(LiteralExpr::new(Some(
            Literal::Boolean(true),
        ))));
        body = Statement::WhileStatement(WhileStmt::new(Box::new(condition), Box::new(body)));

        if let Some(i) = initializer {
            body = Statement::BlockStatement(BlockStmt::new(vec![i, body]));
        }

        Ok((parser, body))
    }

    fn while_statement(self) -> ParsedStatementOrError<'a> {
        let (parser, _) = self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let (parser, condition) = parser.expression()?;
        let (parser, _) = parser.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let (parser, body) = parser.statements()?;

        Ok((
            parser,
            Statement::WhileStatement(WhileStmt::new(Box::new(condition), Box::new(body))),
        ))
    }

    fn if_statement(self) -> ParsedStatementOrError<'a> {
        let (parser, _) = self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let (parser, condition) = parser.expression()?;
        let (parser, _) = parser.consume(TokenType::RightParen, "Expect ')' after 'if'.")?;

        let (parser, then_branch) = parser.statements()?;
        let mut else_branch = None;
        let (mut parser, matched) = parser.match_types(&[TokenType::Else]);
        if matched {
            let (next_parser, statement) = parser.statements()?;
            parser = next_parser;
            else_branch = Some(statement);
        }

        Ok((
            parser,
            Statement::IfStatement(IfStmt::new(
                Box::new(condition),
                Box::new(then_branch),
                Box::new(else_branch),
            )),
        ))
    }

    fn block(self) -> Result<(Parser<'a>, Vec<Statement>), (Parser<'a>, LoxError)> {
        let mut statements = Vec::new();
        let mut parser = self.clone();

        while !parser.check(TokenType::RightBrace) && !parser.is_at_end() {
            let d = parser.declaration();
            match d {
                Ok((next_parser, statement)) => {
                    parser = next_parser;
                    statements.push(statement);
                }
                Err(e) => return Err(e),
            }
        }

        let (next_parser, _) = parser.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok((next_parser, statements))
    }

    fn print_statement(self) -> ParsedStatementOrError<'a> {
        let (parser, expr) = self.expression()?;
        let (parser, _) = parser.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok((
            parser,
            Statement::PrintStatement(PrintStmt::new(Box::new(expr))),
        ))
    }

    fn expression_statement(self) -> ParsedStatementOrError<'a> {
        let expr = self.expression();
        match expr {
            Ok((parser, expression)) => {
                let (parser, _) =
                    parser.consume(TokenType::Semicolon, "Expect ';' after value.")?;
                Ok((
                    parser,
                    Statement::ExpressionStatement(ExpressionStmt::new(Box::new(expression))),
                ))
            }
            Err(e) => Err(e),
        }
    }

    fn expression(self) -> ParsedExpressionOrError<'a> {
        self.assignment()
    }

    fn assignment(self) -> ParsedExpressionOrError<'a> {
        let (parser, expr) = self.or()?;
        let types_to_match = [TokenType::Equal];
        let (parser, matched) = parser.match_types(&types_to_match);
        if matched {
            if let Some(equals) = parser.previous() {
                let (parser, value) = parser.assignment()?;

                match expr {
                    Expression::Variable(v) => {
                        let name = v.name;
                        return Ok((
                            parser,
                            Expression::Assignment(AssignExpr::new(name, Box::new(value))),
                        ));
                    }
                    _ => {
                        return Err((
                            parser.clone(),
                            LoxError::ParseError(
                                parser.error(&equals, "Invalid assignment target."),
                            ),
                        ))
                    }
                }
            }
        }

        return Ok((parser, expr));
    }

    fn or(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.and()?;
        loop {
            let (next_parser, matched) = parser.clone().match_types(&[TokenType::Or]);
            parser = next_parser;

            if matched {
                let operator = parser.previous().unwrap();
                let (next_parser, right) = parser.and()?;
                parser = next_parser;
                expr =
                    Expression::Logical(LogicalExpr::new(Box::new(expr), operator, Box::new(right)))
            } else {
                break;
            }
        }

        Ok((parser, expr))
    }

    fn and(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.equality()?;
        loop {
            let (next_parser, matched) = parser.clone().match_types(&[TokenType::And]);
            parser = next_parser;

            if matched {
                let operator = parser.previous().unwrap();
                let (next_parser, right) = parser.equality()?;
                parser = next_parser;
                expr =
                    Expression::Logical(LogicalExpr::new(Box::new(expr), operator, Box::new(right)))
            } else {
                break;
            }
        }
        Ok((parser, expr))
    }

    fn equality(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.comparison()?;
        let types_to_match = [TokenType::BangEqual, TokenType::EqualEqual];

        loop {
            let (next_parser, matched) = parser.clone().match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap();
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

    fn comparison(self) -> ParsedExpressionOrError<'a> {
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
                let operator = parser.previous().unwrap();
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

    fn term(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.factor()?;
        let types_to_match = [TokenType::Minus, TokenType::Plus];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;

            if matched {
                let operator = parser.previous().unwrap();
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

    fn factor(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.unary()?;
        let types_to_match = [TokenType::Slash, TokenType::Star];

        loop {
            let (next_parser, matched) = parser.match_types(&types_to_match);
            parser = next_parser;
            if matched {
                let operator = parser.previous().unwrap();
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

    fn unary(self) -> ParsedExpressionOrError<'a> {
        let types_to_match = [TokenType::Bang, TokenType::Minus];
        let (parser, matched) = self.match_types(&types_to_match);

        if matched {
            let operator = parser.previous().unwrap();
            let (parser, right) = parser.unary()?;
            Ok((
                parser,
                Expression::Unary(UnaryExpr::new(operator.clone(), Box::new(right))),
            ))
        } else {
            parser.call()
        }
    }

    fn call(self) -> ParsedExpressionOrError<'a> {
        let (mut parser, mut expr) = self.primary()?;

        loop {
            let (next_parser, matched) = parser.clone().match_types(&[TokenType::LeftParen]);
            parser = next_parser;
            if matched {
                let (next_parser, call_expr) = parser.finish_call(&expr)?;
                parser = next_parser;
                expr = call_expr;
            } else {
                break;
            }
        }

        Ok((parser, expr))
    }

    fn finish_call(self, expr: &Expression) -> ParsedExpressionOrError<'a> {
        let mut arguments: Vec<Expression> = Vec::new();
        let mut parser = self;
        if !parser.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let e = parser.error(
                        &parser.peek().unwrap(),
                        "Can't have more than 255 arguments.",
                    );
                    e.report();
                }
                let (next_parser, exp) = parser.expression()?;
                parser = next_parser;
                arguments.push(exp);
                let (next_parser, matched) = parser.match_types(&[TokenType::Comma]);
                parser = next_parser;
                if !matched {
                    break;
                }
            }
        }
        let (parser, t) = parser.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok((
            parser,
            Expression::Call(CallExpr::new(Box::new(expr.clone()), t, arguments)),
        ))
    }

    fn primary(self) -> ParsedExpressionOrError<'a> {
        fn match_false(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) = parser.match_types(&[TokenType::False]);
            if matched {
                (
                    next_parser,
                    Some(Expression::Literal(LiteralExpr::new(Some(
                        Literal::Boolean(false),
                    )))),
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
                    Some(Expression::Literal(LiteralExpr::new(Some(
                        Literal::Boolean(true),
                    )))),
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

        fn match_paren(parser: Parser) -> Result<(Parser, Option<Expression>), (Parser, LoxError)> {
            let (next_parser, matched) = parser.match_types(&[TokenType::LeftParen]);
            if matched {
                let (expr_parser, expr) = next_parser.expression()?;
                let (expr_parser, _t) =
                    expr_parser.consume(TokenType::RightParen, "Expect ')' after expression")?;
                Ok((
                    expr_parser,
                    Some(Expression::Grouping(GroupingExpr::new(Box::new(expr)))),
                ))
            } else {
                Ok((next_parser, None))
            }
        }

        fn match_identifier(parser: Parser) -> (Parser, Option<Expression>) {
            let (next_parser, matched) = parser.match_types(&[TokenType::Identifier]);
            if matched {
                let prev = next_parser
                    .previous()
                    .map(|t| Expression::Variable(VariableExpr::new(t.clone())));
                (next_parser, prev)
            } else {
                (next_parser, None)
            }
        }

        fn process_parser(parser: Parser) -> ParsedExpressionOrError {
            let (next_parser, expression) = parser_util::try_match(parser, match_false);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = parser_util::try_match(next_parser, match_true);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = parser_util::try_match(next_parser, match_nil);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) =
                parser_util::try_match(next_parser, match_alpha_numeric);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = parser_util::try_match(next_parser, match_identifier);
            if let Some(expr) = expression {
                return Ok((next_parser, expr));
            }

            let (next_parser, expression) = match_paren(next_parser)?;
            if let Some(expr) = expression {
                Ok((next_parser, expr))
            } else {
                let tk = next_parser.peek().unwrap();
                Err((
                    next_parser.clone(),
                    LoxError::ParseError(next_parser.error(tk, "Expect expression.")),
                ))
            }
        }

        process_parser(self)
    }

    fn consume(
        self,
        token_type: TokenType,
        message: &str,
    ) -> Result<(Self, Token), (Self, LoxError)> {
        if self.check(token_type) {
            let parser = self.advance();
            let prev = parser.previous().unwrap();
            Ok((parser, prev))
        } else {
            let tk = self.peek().unwrap();
            let parse_error = LoxError::ParseError(self.error(tk, message));
            Err((self, parse_error))
        }
    }

    fn error(&self, tk: &Token, message: &str) -> Error {
        Error::error_with_token(tk, message)
    }

    fn synchronize(self) -> Self {
        let mut next_parser = self.advance();

        while !next_parser.is_at_end() {
            if let Some(t) = next_parser.previous() {
                if *t.get_token_type() == TokenType::Semicolon {
                    return next_parser;
                }
                let peeked = next_parser.peek();
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
                            return next_parser;
                        }
                        _ => {}
                    }
                }
                let aux_parser = next_parser.advance();
                next_parser = aux_parser;
            }
        }
        next_parser
    }

    fn match_types(mut self, types: &[TokenType]) -> (Self, bool) {
        if types.iter().any(|&t| self.check(t)) {
            self = self.advance();
            return (self, true);
        } else {
            return (self, false);
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

    fn previous(&self) -> Option<Token> {
        self.tokens.get((self.current - 1) as usize).cloned()
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

mod parser_util {
    use crate::expression::Expression;

    use super::Parser;

    pub fn try_match<F>(parser: Parser, matcher: F) -> (Parser, Option<Expression>)
    where
        F: FnOnce(Parser) -> (Parser, Option<Expression>),
    {
        matcher(parser)
    }
}
