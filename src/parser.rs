use crate::ast::{AstNode, Operator};
use crate::lexer::Lexer;
use crate::token::TokenType;

pub fn parse(lexer: &mut Lexer) -> AstNode {
    //      parse_expr(lexer, 0)
    expression(lexer)
}
fn expression(lexer: &mut Lexer) -> AstNode {
    let curr = lexer.peek();
    let token_type = curr.token_type;
    match token_type {
        TokenType::If => if_expression(lexer),
        _ => equality(lexer),
    }
}

fn if_expression(lexer: &mut Lexer) -> AstNode {
    let if_tok = lexer.next();
    if if_tok.token_type != TokenType::If {
        panic!("Expected If Token")
    }
    let condition = expression(lexer);
    let then_tok = lexer.next();
    if then_tok.token_type != TokenType::Then {
        panic!("Expected Then Token")
    }
    let if_body = expression(lexer);
    let else_tok = lexer.next();
    if else_tok.token_type != TokenType::Else {
        panic!("Expected Else Token")
    }
    let else_body = expression(lexer);

    AstNode::If {
        condition: Box::new(condition),
        if_body: Box::new(if_body),
        else_body: Box::new(else_body),
    }
}

fn equality(lexer: &mut Lexer) -> AstNode {
    let expr = term(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.next();
            let right = equality(lexer);
            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            }
        }
        _ => expr,
    }
}

fn term(lexer: &mut Lexer) -> AstNode {
    let expr = factor(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.next();
            let right = term(lexer);
            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            }
        }
        _ => expr,
    }
}

fn factor(lexer: &mut Lexer) -> AstNode {
    let expr = unary(lexer);
    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            let right = factor(lexer);
            lexer.next();
            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(expr),
                rhs: Box::new(right),
            }
        }
        _ => expr,
    }
}

fn unary(lexer: &mut Lexer) -> AstNode {
    let token = lexer.peek();
    match token.token_type {
        TokenType::Negation => {
            lexer.next();
            let expr = unary(lexer);
            AstNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(expr),
            }
        }
        _ => primary(lexer),
    }
}

fn primary(lexer: &mut Lexer) -> AstNode {
    let token = lexer.next();
    match token.token_type {
        TokenType::Int(n) => AstNode::Int(n),
        TokenType::Float(n) => AstNode::Float(n),
        TokenType::Bool(b) => AstNode::Bool(b),
        TokenType::Identifier(i) => AstNode::Identifier(i),
        _ => panic!("Expected Literal Value"),
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
