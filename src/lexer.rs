use std::fmt;
use std::fmt::Display;

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    literal: String,
    position: Position,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" {} {}", self.literal, self.token_type, self.position)
    }
}


impl Token {
    pub fn new(literal: String, token_type: TokenType, position: Position) -> Token {
        return Token {
            literal,
            token_type,
            position,
        };
    }
}

#[derive(Debug)]
pub enum TokenType {
    // Single Character Token
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    ForwardSlash,
    // Values
    Float(f64),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LeftParen => write!(f, "LeftParen"),
            Self::RightParen => write!(f, "RightParen"),
            Self::Plus => write!(f, "Plus"),
            Self::Minus => write!(f, "Minus"),
            Self::Star => write!(f, "Star"),
            Self::ForwardSlash => write!(f, "ForwardSlash"),
            Self::Float(n) => write!(f, "Float({})", n),
        }
    }
}

#[derive(Debug)]
pub struct Position {
    start_line: u32,
    end_line: u32,
    start_column: u32,
    end_column: u32,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.start_line, self.start_column)
    }
}

impl Position {
    pub fn new(start_line: u32, end_line: u32, start_column: u32, end_column: u32) -> Position {
        return Position {
            start_line,
            end_line,
            start_column,
            end_column,
        };
    }
    pub fn new_inline(line: u32, start_column: u32, end_column: u32) -> Position {
        return Position::new(line, line, start_column, end_column);
    }
    pub fn new_single_char(line: u32, column: u32) -> Position {
        return Position::new(line, line, column, column);
    }
}

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let lines = text.lines();
    let mut curr_line = 1;
    for line in lines {
        let mut column = 1;
        let mut chars = line.chars().peekable();
        while let Some(character) = chars.peek() {
            match character {
                '(' => {
                    let pos = Position::new_single_char(curr_line, column);
                    let token = Token::new("(".to_string(), TokenType::LeftParen, pos);
                    tokens.push(token);
                    chars.next();
                    column += 1;
                }
                ')' => {
                    let pos = Position::new_single_char(curr_line, column);
                    let token = Token::new(")".to_string(), TokenType::RightParen, pos);
                    tokens.push(token);
                    chars.next();
                    column += 1;
                }
                '+' => {
                    let pos = Position::new_single_char(curr_line, column);
                    let token = Token::new("+".to_string(), TokenType::Plus, pos);
                    tokens.push(token);
                    chars.next();
                    column += 1;
                }
                '-' => {
                    let pos = Position::new_single_char(curr_line, column);
                    let token = Token::new("-".to_string(), TokenType::Minus, pos);
                    tokens.push(token);
                    chars.next();
                    column += 1;
                }
                '*' => {
                    let pos = Position::new_single_char(curr_line, column);
                    let token = Token::new("*".to_string(), TokenType::Star, pos);
                    tokens.push(token);
                    chars.next();
                    column += 1;
                }
                _c => {
                    chars.next();
                }
            }

            curr_line += 1;
        }
    }
    return tokens;
}
