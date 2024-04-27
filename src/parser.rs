use crate::ast::{AstNode, Operator};
use crate::lexer::{Lexer, Token, TokenType};

pub fn parse(lexer: &mut Lexer) -> AstNode {
    //      parse_expr(lexer, 0)
    expression(lexer)
}
fn expression(lexer: &mut Lexer) -> AstNode {
    equality(lexer)
}

fn equality(lexer: &mut Lexer) -> AstNode {
    let expr = term(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.next();
            let right = equality(lexer);
            return AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            };
        }
        _ => return expr,
    }
}

fn term(lexer: &mut Lexer) -> AstNode {
    let expr = factor(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.next();
            let right = term(lexer);
            return AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            };
        }
        _ => return expr,
    }
}

fn factor(lexer: &mut Lexer) -> AstNode {
    let expr = unary(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            let right = factor(lexer);
            lexer.next();
            return AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            };
        }
        _ => return expr,
    }
}

fn unary(lexer: &mut Lexer) -> AstNode {
    let token = lexer.peek();
    match token.token_type {
        TokenType::Negation => {
            lexer.next();
            let expr = unary(lexer);
            return AstNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(expr),
            };
        }
        _ => primary(lexer),
    }
}

fn primary(lexer: &mut Lexer) -> AstNode {
    let token = lexer.next();
    match token.token_type {
        TokenType::Int(n) => AstNode::Int(n),
        TokenType::Real(n) => AstNode::Real(n),
        _ => panic!("Expected Literal Value"),
    }
}

fn get_operator(token_type: TokenType) -> Operator {
    match token_type {
        TokenType::Plus => Operator::Plus,
        TokenType::Minus => Operator::Minus,
        TokenType::Star => Operator::Multiply,
        TokenType::ForwardSlash => Operator::RealDivision,
        TokenType::Negation => Operator::Negation,
        TokenType::EqualEqual => Operator::Equal,
        TokenType::NotEqual => Operator::NotEqual,
        _ => panic!("Invalid Operator"),
    }
}
