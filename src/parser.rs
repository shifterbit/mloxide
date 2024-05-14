use std::fmt::{self, Display};

use crate::ast::{AstNode, Operator};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

type ParseErrorList = Vec<ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    token: Token,
}

impl ParseError {
    fn new(message: &str, token: Token) -> ParseError {
        ParseError {
            message: message.to_string(),
            token,
        }
    }
}

pub fn stringify_parse_errors(errors: ParseErrorList) -> Vec<String> {
    let mut messages: Vec<String> = Vec::new();
    for error in errors {
        messages.push(error.to_string());
    }
    messages
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let position = self.token.position;
        write!(f, "{position} ParseError: {}", self.message)
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<AstNode, Vec<ParseError>> {
    //      parse_expr(lexer, 0)
    let mut errors: Vec<ParseError> = Vec::new();
    expression(lexer, &mut errors)
}
fn expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let curr = lexer.peek();
    let token_type = curr.token_type;
    match token_type {
        TokenType::If => if_expression(lexer, errors),
        TokenType::Eof => Err(errors.clone()),
        _ => equality(lexer, errors),
    }
}

fn if_expression(
    lexer: &mut Lexer,
    errors: &mut Vec<ParseError>,
) -> Result<AstNode, ParseErrorList> {
    let if_tok = lexer.next();
    if if_tok.token_type != TokenType::If {
        let error_val = ParseError::new("expected token if", if_tok);
        errors.push(error_val);
        return Err(errors.clone());
    }
    let condition = expression(lexer, errors);
    let condition_expr: AstNode = match condition {
        Ok(expr) => expr,
        Err(errors) => return Err(errors.clone()),
    };

    let then_tok = lexer.next();
    if then_tok.token_type != TokenType::Then {
        let error_val = ParseError::new("expected token then", then_tok);
        errors.push(error_val);
        return Err(errors.clone());
    }

    let if_body = expression(lexer, errors);
    let if_expr: AstNode = match if_body {
        Ok(expr) => expr,
        Err(errors) => return Err(errors),
    };

    let else_tok = lexer.next();
    if else_tok.token_type != TokenType::Else {
        let error_val = ParseError::new("expected token else", else_tok);
        errors.push(error_val);
        return Err(errors.clone());
    }
    let else_body = expression(lexer, errors);
    let else_expr: AstNode = match else_body {
        Ok(expr) => expr,
        Err(errors) => return Err(errors),
    };

    Ok(AstNode::If {
        condition: Box::new(condition_expr),
        if_body: Box::new(if_expr),
        else_body: Box::new(else_expr),
    })
}

fn equality(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let expr = term(lexer, errors);
    let lhs: AstNode = match expr {
        Ok(e) => e,
        Err(errs) => return Err(errs),
    };

    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.next();
            let right = equality(lexer, errors);
            let rhs: AstNode = match right {
                Ok(e) => e,
                Err(e) => return Err(e),
            };

            Ok(AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
        _ => Ok(lhs),
    }
}

fn term(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let expr = factor(lexer, errors);
    let lhs: AstNode = match expr {
        Ok(e) => e,
        Err(e) => return Err(e),
    };

    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.next();
            let right = term(lexer, errors);
            let rhs: AstNode = match right {
                Ok(e) => e,
                Err(e) => return Err(e),
            };

            Ok(AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
        _ => Ok(lhs),
    }
}

fn factor(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let expr = unary(lexer, errors);
    let lhs: AstNode = match expr {
        Ok(e) => e,
        Err(e) => return Err(e),
    };

    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            lexer.next();
            let right = factor(lexer, errors);
            let rhs: AstNode = match right {
                Ok(e) => e,
                Err(e) => return Err(e),
            };
            Ok(AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
        _ => Ok(lhs),
    }
}

fn unary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let token = lexer.peek();
    match token.token_type {
        TokenType::Negation => {
            lexer.next();
            let expr = unary(lexer, errors);
            let right_expr: AstNode = match expr {
                Ok(e) => e,
                Err(e) => return Err(e),
            };

            Ok(AstNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(right_expr),
            })
        }
        _ => primary(lexer, errors),
    }
}

fn primary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Result<AstNode, ParseErrorList> {
    let token = lexer.next();
    match token.token_type {
        TokenType::Int(n) => Ok(AstNode::Int(n)),
        TokenType::Float(n) => Ok(AstNode::Float(n)),
        TokenType::Bool(b) => Ok(AstNode::Bool(b)),
        TokenType::Identifier(i) => Ok(AstNode::Identifier(i)),
        TokenType::LeftParen => {
            let expr = expression(lexer, errors);
            let inner_expr: AstNode = match expr {
                Ok(e) => e,
                Err(e) => return Err(e),
            };

            if lexer.next().token_type == TokenType::RightParen {
                Ok(AstNode::Grouping(Box::new(inner_expr)))
            } else {
                let error = ParseError::new("Expected closing parenthesis", lexer.peek());
                errors.push(error);
                Err(errors.clone())
            }
        }
        _ => panic!("Expected literal value"),
    }
}

fn get_operator(token_type: TokenType) -> Operator {
    match token_type {
        TokenType::Plus => Operator::Add,
        TokenType::Minus => Operator::Subtract,
        TokenType::Star => Operator::Multiply,
        TokenType::ForwardSlash => Operator::Divide,
        TokenType::Negation => Operator::Negate,
        TokenType::EqualEqual => Operator::Equal,
        TokenType::NotEqual => Operator::NotEqual,
        _ => panic!("Invalid Operator"),
    }
}
