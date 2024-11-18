use crate::scanner::token::{Literal, Token};

pub trait Expr {
    fn parenthesize(&self, name: &str, expressions: &Vec<&Expression>) -> String {
        let mut s = String::new();
        s.push('(');
        s.push_str(name);
        for expr in expressions {
            s.push(' ');
            let expr_str: String = match expr {
                Expression::Binary(binary_expr) => self.parenthesize(
                    binary_expr.operator.get_lexeme(),
                    &vec![&binary_expr.left, &binary_expr.right],
                ),
                Expression::Grouping(grouping_expr) => {
                    self.parenthesize("group", &vec![&grouping_expr.expression])
                }
                Expression::Literal(literal_expr) => {
                    let cloned_literal = literal_expr.literal.clone();
                    cloned_literal.map_or(String::from("Nil"), |x| x.to_string())
                }
                Expression::Unary(literal_expr) => self.parenthesize(
                    literal_expr.operator.get_lexeme(),
                    &vec![&literal_expr.right],
                ),
            };
            s.push_str(&expr_str);
        }
        s.push(')');
        s
    }
}

#[macro_export]
macro_rules! define_expr {
    ($name:ident, $($field_name:ident: $field_type:ty),*) => {
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

        impl Expr for $name {}
    };
}

pub enum Expression {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<UnaryExpr>),
}

define_expr!(BinaryExpr, left: Expression, operator: Token, right: Expression);
define_expr!(GroupingExpr, expression: Expression);
define_expr!(LiteralExpr, literal: Option<Literal>);
define_expr!(UnaryExpr, operator: Token, right: Expression);
