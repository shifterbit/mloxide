use crate::lexer::{Lexer, Token, TokenType};

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Negation,
    IntDivision,
    RealDivision,
    Multiply,
}
#[derive(Debug)]
pub enum AstNode {
    Int(i64),
    Float(f64),
    Binary {
        op: Operator,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Unary {
        op: Operator,
        expr: Box<AstNode>,
    },
}

pub fn parse(lexer: &mut Lexer) -> AstNode {
    parse_expr(lexer, 0)
}

fn parse_expr(mut lexer: &mut Lexer, min_bp: u8) -> AstNode {
    // Get left hand side
    let mut lhs: AstNode = match lexer.peek() {
        Token {
            token_type: TokenType::Int(num),
            ..
        } => {
            lexer.next();
            AstNode::Int(num)
        }
        Token {
            token_type: TokenType::Float(num),
            ..
        } => {
            lexer.next();
            AstNode::Float(num)
        }
        Token {
            token_type: TokenType::Negation,
            ..
        } => {
            let ((), r_bp) = prefix_binding_power(Operator::Negation);
            lexer.next();
            let rhs = parse_expr(lexer, r_bp);
            AstNode::Unary { op: Operator::Negation, expr: Box::new(rhs) }
        },
        t => panic!("Bad Token"),
    };

    loop {
        // Look for next Operator
        let op = match lexer.peek() {
            Token {
                token_type: TokenType::Eof,
                ..
            } => break,
            Token {
                token_type: TokenType::Plus,
                ..
            } => Operator::Plus,
            Token {
                token_type: TokenType::Minus,
                ..
            } => Operator::Minus,
            Token {
                token_type: TokenType::ForwardSlash,
                ..
            } => Operator::IntDivision,
            Token {
                token_type: TokenType::Star,
                ..
            } => Operator::Multiply,
            _ => panic!("Bad Token"), // No Known Operator Found
        };

        let (l_bp, r_bp) = infix_binding_power(op);
        // Check precedence of LHS
        if l_bp < min_bp {
            break; // Return lhs since pecedence is too low
        }
        lexer.next();
        // Parse Right Hand Side
        let rhs = parse_expr(lexer, r_bp);
        // Return new Binary Expression
        lhs = AstNode::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    lhs
}

fn infix_binding_power(op: Operator) -> (u8, u8) {
    match op {
        Operator::Plus | Operator::Minus => (1, 2),
        Operator::Multiply | Operator::IntDivision | Operator::RealDivision => (3, 4),
        _ => (0, 1),
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Negation => ((), 8),
        _ => panic!("Bad Operator"),
    }
}
