use std::{env, fs};

fn main() {
    println!("Hello, world!");
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let text = fs::read_to_string(file_path);
    tokenize(text.unwrap());
}

fn tokenize(text: String) -> Vec<Token> {
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

                _c => todo!(),
            }

            curr_line += 1;
        }
    }
    return tokens;
}

struct Token {
    token_type: TokenType,
    literal: String,
    position: Position,
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

enum TokenType {
    // Single Character Token
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    // Values
    Int(i32),
    // Builtin Multi-Character Infix
    IntDiv,
}

struct Position {
    start_line: u32,
    end_line: u32,
    start_column: u32,
    end_column: u32,
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
