use std::fmt;

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
            TokenType::And => "And",
            TokenType::Bang => "Bang",
            TokenType::BangEqual => "BangEqual",
            TokenType::Class => "Class",
            TokenType::Comma => "Comma",
            TokenType::Dot => "Dot",
            TokenType::Else => "Else",
            TokenType::Eof => "Eof",
            TokenType::Equal => "Equal",
            TokenType::EqualEqual => "EqualEqual",
            TokenType::False => "False",
            TokenType::For => "For",
            TokenType::Fun => "Fun",
            TokenType::Greater => "Greater",
            TokenType::GreaterEqual => "GreaterEqual",
            TokenType::Identifier => "Identifier",
            TokenType::If => "If",
            TokenType::LeftBrace => "LeftBrace",
            TokenType::LeftParen => "LeftParen",
            TokenType::Less => "Less",
            TokenType::LessEqual => "LessEqual",
            TokenType::Minus => "Minus",
            TokenType::Nil => "Nil",
            TokenType::Number => "Number",
            TokenType::Or => "Or",
            TokenType::Plus => "Plus",
            TokenType::Print => "Print",
            TokenType::Return => "Return",
            TokenType::RightBrace => "RightBrace",
            TokenType::RightParen => "RightParen",
            TokenType::Semicolon => "Semicolon",
            TokenType::Slash => "Slash",
            TokenType::Star => "Star",
            TokenType::String => "String",
            TokenType::Super => "Super",
            TokenType::This => "This",
            TokenType::True => "True",
            TokenType::Var => "Var",
            TokenType::While => "While",
        };
        write!(f, "{}", type_str)
    }
}
