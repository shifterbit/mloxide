use std::iter::Peekable;
use crate::token::{Position, Token, TokenType};


#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(text: &str) -> Lexer {
        let mut tokens: Vec<Token> = Vec::new();
        let mut line = 1;
        let mut column = 1;
        let mut chars = text.chars().peekable();
        while let Some(character) = chars.peek() {
            match character {
                '\n' => {
                    line += 1;
                    column = 1;
                    chars.next();
                }
                ' ' => {
                    column += 1;
                    chars.next();
                }
                '\t' => {
                    column += 4;
                    chars.next();
                }
                '=' | '!' => {
                    let literal = read_multi_character_token(&mut chars);
                    let length = literal.len() as u32;
                    let token = match_multi_character_token(&literal, line, column);
                    tokens.push(token);
                    column += length;
                    chars.next();
                }
                '(' | ')' | '+' | '-' | '*' | '/' | '~' => {
                    let token = match_single_character_token(*character, line, column);
                    tokens.push(token.unwrap());
                    column += 1;
                    chars.next();
                }
                _c if is_digit(character) => {
                    let literal = read_number(&mut chars);
                    let length = literal.len() as u32;
                    let token = match_number(&literal, line, column);
                    tokens.push(token);
                    column += length;
                }
                _c if is_alpha(character) => {
                    let literal = read_indentifier(&mut chars);
                    let length = literal.len() as u32;
                    let token_type = match_keywords(&literal);
                    let token = Token {token_type, literal, position: Position::new(line, column)};
                    tokens.push(token);
                    column += length;
                }
                _ => {
                    column += 1;
                    chars.next();
                }
            }
        }
        tokens.reverse();
        Lexer { tokens }
    }
    pub fn peek(self: &Lexer) -> Token {
        let token = self.tokens.last().cloned().unwrap_or_default();
        token
    }
    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or_default()
    }
}

static LETTERS: [char; 52] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

static DIGITS: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

fn read_indentifier<I: Iterator<Item = char>>(chars: &mut Peekable<I>) -> String {
    let mut literal: String = "".to_string();
    while let Some(c) = chars.peek() {
        if LETTERS.contains(c) || DIGITS.contains(c) || *c == '_' {
            literal.push(c.to_owned());
            chars.next();
        } else {
            break;
        }
    }
    literal
}

fn match_keywords(literal: &str) -> TokenType {
    match literal {
        "true" => TokenType::Bool(true),
        "false" => TokenType::Bool(false),
        "if" => TokenType::If,
        "then" => TokenType::Then,
        "else" => TokenType::Else,
        _ => TokenType::Identifier(literal.to_owned()),
    }
}

fn is_alpha(character: &char) -> bool {
    LETTERS.contains(character)
}

fn is_digit(character: &char) -> bool {
    DIGITS.contains(character)
}

fn read_number<I: Iterator<Item = char>>(chars: &mut Peekable<I>) -> String {
    let mut num_str: String = "".to_string();
    let mut found_decimal = false;
    while let Some(c) = chars.peek() {
        match c {
            ch if is_digit(c) => {
                num_str.push(ch.to_owned());
                chars.next();
            }
            '.' if !found_decimal => {
                num_str.push('.');
                found_decimal = true;
                chars.next();
            }
            _ => break,
        }
    }
    num_str
}

fn match_number(num_str: &str, line: u32, column: u32) -> Token {
    let position = Position::new(line, column);
    if num_str.contains('.') {
        let float_val: f64 = num_str.parse().unwrap();
        Token::new(num_str.to_string(), TokenType::Float(float_val), position)
    } else {
        let int_val: i64 = num_str.parse().unwrap();
        
        Token::new(num_str.to_string(), TokenType::Int(int_val), position)
    }
}

fn read_multi_character_token<I: Iterator<Item = char>>(chars: &mut Peekable<I>) -> String {
    let mut literal: String = chars.peek().unwrap().to_string();

    if let Some('=') = chars.next() {
        literal.push('=');
    };
    literal
}

fn match_multi_character_token(literal: &str, line: u32, column: u32) -> Token {
    match literal {
        "!=" => {
            Token {
                token_type: TokenType::NotEqual,
                literal: literal.to_owned(),
                position: Position::new(line, column),
            }
        }
        "==" => {
            Token {
                token_type: TokenType::EqualEqual,
                literal: literal.to_owned(),
                position: Position::new(line, column),
            }
        }
        _ => {
            panic!("Unexpected Character")
        }
    }
}

#[derive(Debug)]
struct InvalidTokenError;
fn match_single_character_token(
    character: char,
    line: u32,
    column: u32,
) -> Result<Token, InvalidTokenError> {
    let position = Position::new(line, column);
    let token_type = match character {
        '(' => TokenType::LeftParen,
        ')' => TokenType::RightParen,
        '-' => TokenType::Minus,
        '+' => TokenType::Plus,
        '*' => TokenType::Star,
        '/' => TokenType::ForwardSlash,
        '~' => TokenType::Negation,
        _ => return Err(InvalidTokenError),
    };
    let token = Token::new(character.to_string(), token_type, position);
    Ok(token)
}
