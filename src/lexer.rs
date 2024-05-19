use crate::token::{Token, TokenType};
use std::iter::Peekable;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    position: usize,
    eof_token: Token,
}

impl Lexer {
    pub fn new(text: &str) -> Lexer {
        let mut tokens: Vec<Token> = Vec::new();
        let mut offset: usize = 0;
        let mut chars = text.chars().peekable();
        while let Some(character) = chars.peek() {
            match character {
                '\n' | ' ' | '\t' => {
                    offset += 1;
                    chars.next();
                    if chars.peek().is_none() {
                        offset -= 1;
                    }
                }
                '=' | '!' => {
                    offset += 1;
                    let literal = read_multi_character_token(&mut chars);
                    let length = literal.len();
                    let token = match_multi_character_token(&literal, offset);
                    tokens.push(token.clone());
                    offset += length;
                    chars.next();
                }
                '(' | ')' | '+' | '-' | '*' | '/' | '~' | ';' => {
                    let token = match_single_character_token(*character, offset);
                    tokens.push(token.unwrap());
                    offset += 1;
                    chars.next();
                }
                _c if is_digit(character) => {
                    offset += 1;
                    let literal = read_number(&mut chars);
                    let length = literal.len();
                    let token = match_number(&literal, offset);
                    tokens.push(token.clone());
                    offset += length;
                }
                _c if is_alpha(character) => {
                    let literal = read_indentifier(&mut chars);
                    let length = literal.len();
                    let token_type = match_keywords(&literal);
                    let token = Token {
                        token_type,
                        literal,
                        offset,
                    };
                    tokens.push(token);
                    offset += length;
                }
                _ => {
                    offset += 1;
                    chars.next();
                }
            }
        }

        let eof_token = Token {
            token_type: TokenType::Eof,
            literal: "".to_string(),
            offset: (offset - 1),
        };
        Lexer {
            tokens,
            eof_token,
            position: 0,
        }
    }
    pub fn peek(&self) -> Token {
        if self.position < self.tokens.len() {
            self.tokens[self.position].clone()
        } else {
            self.eof_token.clone()
        }
    }
    pub fn previous(&self) -> Token {
        self.tokens[self.position - 1].clone()
    }
    pub fn next(&mut self) -> Token {
        if self.position < self.tokens.len() {
            let tok = self.tokens[self.position].clone();
            self.position += 1;
            tok
        } else {
            self.eof_token.clone()
        }
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
    while let Some(c) = chars.next() {
        literal.push(c.to_owned());

        if let Some(c) = chars.peek() {
            if !(LETTERS.contains(c) || DIGITS.contains(c) || *c == '_') {
                break;
            }
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
        "val" => TokenType::Val,
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

fn match_number(num_str: &str, offset: usize) -> Token {
    if num_str.contains('.') {
        let float_val: f64 = num_str.parse().unwrap();
        Token::new(num_str.to_string(), TokenType::Float(float_val), offset)
    } else {
        let int_val: i64 = num_str.parse().unwrap();

        Token::new(num_str.to_string(), TokenType::Int(int_val), offset)
    }
}

fn read_multi_character_token<I: Iterator<Item = char>>(chars: &mut Peekable<I>) -> String {
    let mut literal: String = chars.next().unwrap().to_string();

    if let Some('=') = chars.peek() {
        literal.push('=');
    };
    literal
}

fn match_multi_character_token(literal: &str, offset: usize) -> Token {
    match literal {
        "!=" => Token {
            token_type: TokenType::NotEqual,
            literal: literal.to_owned(),
            offset,
        },
        "==" => Token {
            token_type: TokenType::EqualEqual,
            literal: literal.to_owned(),
            offset,
        },
        "=" => Token {
            token_type: TokenType::Equal,
            literal: literal.to_owned(),
            offset,
        },
        _ => {
            panic!("Unexpected Character")
        }
    }
}

#[derive(Debug)]
struct InvalidTokenError;
fn match_single_character_token(
    character: char,
    offset: usize,
) -> Result<Token, InvalidTokenError> {
    let token_type = match character {
        '(' => TokenType::LeftParen,
        ')' => TokenType::RightParen,
        '-' => TokenType::Minus,
        '+' => TokenType::Plus,
        '*' => TokenType::Star,
        '/' => TokenType::ForwardSlash,
        '~' => TokenType::Negation,
        ';' => TokenType::Semicolon,
        _ => return Err(InvalidTokenError),
    };
    let token = Token::new(character.to_string(), token_type, offset);
    Ok(token)
}
