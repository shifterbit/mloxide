use std::fmt::{self, Display};

use crate::source_location::{SourceLocation, SourcePosition};

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub offset: usize,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            literal: "".to_string(),
            token_type: TokenType::Eof,
            offset: 0,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" {} {}",
            self.literal, self.token_type, self.offset
        )
    }
}

impl Token {
    pub fn new(literal: String, token_type: TokenType, offset: usize) -> Token {
        Token {
            literal,
            token_type,
            offset,
        }
    }
}

impl SourcePosition for Token {
    fn source_location(&self) -> SourceLocation {
        let token = self;
        let literal = &token.literal;
        let start = token.offset;
        let end = start + literal.len();
        SourceLocation::new(start, end)
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

    // Let Expressions
    Let,
    In,
    End,

    // EOF
    Eof,
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
            TokenType::Let => write!(f, "Let"),
            TokenType::In => write!(f, "End"),
            TokenType::End => write!(f, "End"),
            TokenType::Identifier(i) => write!(f, "Identifier({})", i),
        }
    }
}
