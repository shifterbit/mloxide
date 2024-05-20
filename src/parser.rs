use crate::ast::{AstNode, Operator};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::fmt::{self, Display};

pub type ParseErrorList = Vec<ParseError>;

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
    pub fn start(&self) -> usize {
        self.token.offset
    }
    pub fn end(&self) -> usize {
        self.token.offset + self.token.literal.len()
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<AstNode, Vec<ParseError>> {
    //      parse_expr(lexer, 0)
    let mut errors: Vec<ParseError> = Vec::new();
    declarations(lexer, &mut errors)
}

fn recover_from_error(lexer: &mut Lexer) {
    while lexer.peek().token_type != TokenType::Eof {
        match lexer.peek().token_type {
            TokenType::Val => {
                break;
            }
            _ => {
                lexer.next();
            }
        }
    }
}

fn expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let curr = lexer.peek();
    let token_type = curr.token_type;
    match token_type {
        TokenType::If => if_expression(lexer, errors),
        TokenType::Eof => AstNode::Error,
        _ => equality(lexer, errors),
    }
}

fn declarations(
    lexer: &mut Lexer,
    errors: &mut Vec<ParseError>,
) -> Result<AstNode, ParseErrorList> {
    let mut declarations: Vec<AstNode> = Vec::new();
    let mut found_error = false;
    while lexer.peek().token_type != TokenType::Eof {
        match lexer.peek().token_type {
            TokenType::Val => {
                let var_declaration = variable_declaration(lexer, errors);
                declarations.push(var_declaration.clone());
                match var_declaration {
                    AstNode::VariableDeclaration { variable: _, value } => {
                        if let AstNode::Error = *value.clone() {
                            found_error = true;
                            recover_from_error(lexer);
                        }
                    }
                    AstNode::Error => {
                        found_error = true;
                        recover_from_error(lexer);
                    }
                    _ => {
                        panic!("This should not happen");
                    }
                };
            }
            _ => {
                let expr = expression(lexer, errors);
                declarations.push(expr);
            }
        }
    }
    if found_error {
        Err(errors.to_vec())
    } else {
        Ok(AstNode::Declarations(declarations))
    }
}

fn variable_declaration(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let val_tok = lexer.next();
    if val_tok.token_type != TokenType::Val {
        let error_val = ParseError::new("variable declarations should start with 'val'", val_tok);
        errors.push(error_val);
        return AstNode::Error;
    }

    let id_tok = lexer.peek();
    let variable_name = match id_tok.clone().token_type {
        TokenType::Identifier(name) => name,
        _ => {
            let error_val = ParseError::new("expected identifier after val", lexer.previous());
            errors.push(error_val);
            return AstNode::Error;
        }
    };
    lexer.next();

    let eq_tok = lexer.peek();
    if eq_tok.token_type != TokenType::Equal {
        let error_val = ParseError::new(
            &format!("'=' expected after {variable_name}"),
            lexer.previous(),
        );
        errors.push(error_val);
        return AstNode::Error;
    }
    lexer.next();

    if [TokenType::Eof, TokenType::Val, TokenType::Semicolon].contains(&lexer.peek().token_type) {
        let error_val = ParseError::new("Expect expression after =", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }
    let assigned_expr = expression(lexer, errors);

    let semicolon_tok = lexer.peek();
    if semicolon_tok.token_type != TokenType::Semicolon {
        let error_val =
            ParseError::new("expected ';' after variable declaration", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }
    lexer.next();

    AstNode::VariableDeclaration {
        variable: variable_name,
        value: Box::new(assigned_expr),
    }
}

fn if_expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let if_tok = lexer.peek();
    if if_tok.token_type != TokenType::If {
        let error_val = ParseError::new("expected token if", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }
    lexer.next();

    let condition = expression(lexer, errors);
    if let AstNode::Error = condition {
        let error_val = ParseError::new("expected expression after if", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }

    let then_tok = lexer.peek();
    if then_tok.token_type != TokenType::Then {
        let error_val = ParseError::new("expected token then", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }
    lexer.next();

    let if_body = expression(lexer, errors);
    if let AstNode::Error = condition {
        let error_val = ParseError::new("expected expression after then", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }

    let else_tok = lexer.peek();
    if else_tok.token_type != TokenType::Else {
        let error_val = ParseError::new("expected token else", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }
    lexer.next();
    let else_body = expression(lexer, errors);
    if let AstNode::Error = condition {
        let error_val = ParseError::new("expected expression after else", lexer.previous());
        errors.push(error_val);
        return AstNode::Error;
    }

    AstNode::If {
        condition: Box::new(condition),
        if_body: Box::new(if_body),
        else_body: Box::new(else_body),
    }
}

fn equality(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let lhs = term(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.next();
            let rhs = equality(lexer, errors);

            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        _ => lhs,
    }
}

fn term(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let lhs = factor(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.next();
            let rhs = term(lexer, errors);

            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        _ => lhs,
    }
}

fn factor(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let lhs = unary(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            lexer.next();
            let rhs = factor(lexer, errors);
            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        _ => lhs,
    }
}

fn unary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let token = lexer.peek();
    match token.token_type {
        TokenType::Negation => {
            lexer.next();
            let expr = unary(lexer, errors);

            AstNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(expr),
            }
        }
        _ => primary(lexer, errors),
    }
}

fn primary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let token = lexer.next();
    match token.token_type {
        TokenType::Int(n) => AstNode::Int(n),
        TokenType::Float(n) => AstNode::Float(n),
        TokenType::Bool(b) => AstNode::Bool(b),
        TokenType::Identifier(i) => AstNode::Identifier(i),
        TokenType::LeftParen => {
            let expr = expression(lexer, errors);

            if lexer.next().token_type == TokenType::RightParen {
                AstNode::Grouping(Box::new(expr))
            } else {
                let error = ParseError::new("Expected closing parenthesis", lexer.peek());
                errors.push(error);
                AstNode::Error
            }
        }
        _ => {
            let error = ParseError::new("expected a literal value", lexer.peek());
            errors.push(error);
            AstNode::Error
        }
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
