use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub position: Position,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            literal: "".to_string(),
            token_type: TokenType::Eof,
            position: Position::new(0, 0),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" {} {}",
            self.literal, self.token_type, self.position
        )
    }
}

impl Token {
    pub fn new(literal: String, token_type: TokenType, position: Position) -> Token {
        Token {
            literal,
            token_type,
            position,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single Character Token
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Negation,
    Equal,
    Semicolon,
    
    // Multi Character Tokens
    EqualEqual,
    NotEqual,
    // Values
    Float(f64),
    Int(i64),
    Bool(bool),

    // Identifier
    Identifier(String),

    // Conditionals
    If,
    Then,
    Else,

    // Declarations
    Val,

    // EOF
    Eof
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Star => write!(f, "Star"),
            TokenType::ForwardSlash => write!(f, "ForwardSlash"),
            TokenType::Negation => write!(f, "Negation"),
            TokenType::Float(n) => write!(f, "Float({})", n),
            TokenType::Int(n) => write!(f, "Int({})", n),
            TokenType::Eof => write!(f, "EOF"),
            TokenType::Equal => write!(f, "Equal"),
            TokenType::EqualEqual => write!(f, "EqualEqual"),
            TokenType::NotEqual => write!(f, "NotEqual"),
            TokenType::Bool(b) => write!(f, "Bool({})", b),
            TokenType::Semicolon => write!(f, "Semicolon"),
            TokenType::If => write!(f, "If"),
            TokenType::Then => write!(f, "Then"),
            TokenType::Else => write!(f, "Else"),
            TokenType::Val => write!(f, "Val"),
            TokenType::Identifier(i) => write!(f, "Identifier({})", i),
            
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    line: u32,
    column: u32,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Position {
    pub fn new(line: u32, start_column: u32) -> Position {
        Position {
            line,
            column: start_column,
        }
    }
}
