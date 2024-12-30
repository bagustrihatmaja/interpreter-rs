use crate::token::{Literal, Token};

#[macro_export]
macro_rules! define_expr {
    ($name:ident, $($field_name:ident: $field_type:ty),*) => {
        #[derive(Clone, Debug)]
        pub struct $name {
            $(pub $field_name: $field_type),*
        }

        impl $name  {
            pub fn new($($field_name: $field_type),*) -> $name {
                Self {
                    $($field_name),*
                }
            }
        }
    };
}

#[derive(Clone, Debug)]
pub enum Expression {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Assignment(AssignExpr),
}

#[derive(Clone, Debug)]
pub enum Statement {
    PrintStatement(PrintExpr),
    ExpressionStatement(ExpressionExpr),
    VarStatement(VarExpr),
}

impl Expression {
    fn parenthesize(&self, name: &str, expressions: &Vec<&Expression>) -> String {
        let mut s = String::new();
        s.push('(');
        s.push_str(name);
        for expr in expressions {
            s.push(' ');
            let expr_str: String = expr.visit();
            s.push_str(&expr_str);
        }
        s.push(')');
        s
    }

    pub fn visit(&self) -> String {
        match self {
            Expression::Binary(binary_expr) => self.parenthesize(
                binary_expr.operator.get_lexeme(),
                &vec![&binary_expr.left, &binary_expr.right],
            ),
            Expression::Grouping(grouping_expr) => {
                self.parenthesize("group", &vec![&grouping_expr.expression])
            }
            Expression::Literal(literal_expr) => {
                let cloned_literal = literal_expr.literal.clone();
                cloned_literal.map_or(String::from("nil"), |x| x.to_string())
            }
            Expression::Unary(literal_expr) => self.parenthesize(
                literal_expr.operator.get_lexeme(),
                &vec![&literal_expr.right],
            ),
            Expression::Variable(variable_expr) => {
                self.parenthesize(variable_expr.name.get_lexeme(), &vec![])
            }
            Expression::Assignment(assign_expr) => {
                self.parenthesize(assign_expr.name.get_lexeme(), &vec![&assign_expr.value])
            }
        }
    }
}

define_expr!(BinaryExpr, left: Box<Expression>, operator: Token, right: Box<Expression>);
define_expr!(GroupingExpr, expression: Box<Expression>);
define_expr!(LiteralExpr, literal: Option<Literal>);
define_expr!(UnaryExpr, operator: Token, right: Box<Expression>);
define_expr!(ExpressionExpr, expression: Box<Expression>);
define_expr!(PrintExpr, expression: Box<Expression>);
define_expr!(VarExpr, name: Token, initializer: Option<Expression>);
define_expr!(VariableExpr, name: Token);
define_expr!(AssignExpr, name: Token, value: Box<Expression>);

#[cfg(test)]
mod tests {
    use crate::token_type::TokenType;

    use super::*;

    #[test]
    fn build_ast() {
        let ast = Expression::Binary(BinaryExpr::new(
            Box::new(Expression::Unary(UnaryExpr::new(
                Token::new(TokenType::Minus, "-".into(), None, 1),
                Box::new(Expression::Literal(LiteralExpr::new(Some(
                    Literal::Double(123.0),
                )))),
            ))),
            Token::new(TokenType::Star, "*".into(), None, 1),
            Box::new(Expression::Grouping(GroupingExpr::new(Box::new(
                Expression::Literal(LiteralExpr::new(Some(Literal::Double(45.67)))),
            )))),
        ));
        let result = ast.visit();
        assert_eq!(result, "(* (- 123) (group 45.67))")
    }
}
