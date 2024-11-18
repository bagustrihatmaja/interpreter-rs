use crate::scanner::token::{Literal, Token};

#[macro_export]
macro_rules! define_expr {
    ($name:ident, $($field_name:ident: $field_type:ty),*) => {
        pub struct $name {
            $(pub $field_name: $field_type),*
        }

        impl  $name {
            fn new($($field_name: $field_type),*) -> $name {
                Self {
                    $($field_name),*
                }
            }
        }
    };
}

pub enum Expression {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<LiteralExpr>),
}

define_expr!(BinaryExpr, left: Expression, operator: Token, right: Expression);
define_expr!(GroupingExpr, expression: Expression);
define_expr!(LiteralExpr, literal: Literal);
define_expr!(UnaryExpr, operator: Token, right: Expression);
