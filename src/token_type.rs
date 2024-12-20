use std::fmt;

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_str = match self {
            TokenType::And => "AND",
            TokenType::Bang => "BANG",
            TokenType::BangEqual => "BANG_EQUAL",
            TokenType::Class => "CLASS",
            TokenType::Comma => "COMMA",
            TokenType::Dot => "DOT",
            TokenType::Else => "ELSE",
            TokenType::Eof => "EOF",
            TokenType::Equal => "EQUAL",
            TokenType::EqualEqual => "EQUAL_EQUAL",
            TokenType::False => "FALSE",
            TokenType::For => "FOR",
            TokenType::Fun => "FUN",
            TokenType::Greater => "GREATER",
            TokenType::GreaterEqual => "GREATER_EQUAL",
            TokenType::Identifier => "IDENTIFIER",
            TokenType::If => "IF",
            TokenType::LeftBrace => "LEFT_BRACE",
            TokenType::LeftParen => "LEFT_PAREN",
            TokenType::Less => "LESS",
            TokenType::LessEqual => "LESS_EQUAL",
            TokenType::Minus => "MINUS",
            TokenType::Nil => "NIL",
            TokenType::Number => "NUMBER",
            TokenType::Or => "OR",
            TokenType::Plus => "PLUS",
            TokenType::Print => "PRINT",
            TokenType::Return => "RETURN",
            TokenType::RightBrace => "RIGHT_BRACE",
            TokenType::RightParen => "RIGHT_PAREN",
            TokenType::Semicolon => "SEMICOLON",
            TokenType::Slash => "SLASH",
            TokenType::Star => "STAR",
            TokenType::String => "STRING",
            TokenType::Super => "SUPER",
            TokenType::This => "THIS",
            TokenType::True => "TRUE",
            TokenType::Var => "VAR",
            TokenType::While => "WHILE",
        };
        write!(f, "{}", type_str)
    }
}
