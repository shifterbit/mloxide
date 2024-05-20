use crate::ast::{AstNode, Operator};
use crate::error_reporting::CompilerError;
use crate::lexer::Lexer;
use crate::source_location::{SourceLocation, SourcePosition};
use crate::token::TokenType;
use std::fmt::{self, Display};

pub type ParseErrorList = Vec<ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    location: SourceLocation,
}

impl CompilerError for ParseError {
    fn new(message: &str, location: SourceLocation) -> ParseError {
        ParseError {
            message: message.to_string(),
            location,
        }
    }
    fn location(&self) -> SourceLocation {
        self.location
    }
    fn message(&self) -> &str {
        &self.message
    }
    fn error_type(&self) -> &str {
        "ParseError"
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<AstNode, Vec<ParseError>> {
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
    let token_type = curr.token_type.clone();
    let start = curr.source_location().start;

    match token_type {
        TokenType::If => if_expression(lexer, errors),
        TokenType::Eof => {
            let location = SourceLocation::new(start, curr.source_location().end);
            AstNode::Error(location)
        }
        _ => equality(lexer, errors),
    }
}

fn declarations(
    lexer: &mut Lexer,
    errors: &mut Vec<ParseError>,
) -> Result<AstNode, ParseErrorList> {
    let mut declarations: Vec<AstNode> = Vec::new();
    let declarations_loc_start = lexer.peek().source_location();
    let start = declarations_loc_start.start;
    let mut end = declarations_loc_start.end;
    while lexer.peek().token_type != TokenType::Eof {
        match lexer.peek().token_type {
            TokenType::Val => {
                let var_declaration = variable_declaration(lexer, errors);
                declarations.push(var_declaration.clone());
                if let AstNode::Error(_) = var_declaration {
                    recover_from_error(lexer);
                }
            }
            _ => {
                let expr = expression(lexer, errors);
                end = lexer.peek().source_location().end;
                declarations.push(expr);
            }
        }
    }

    if errors.is_empty() {
        let location = SourceLocation::new(start, end);
        Ok(AstNode::Declarations(declarations, location))
    } else {
        Err(errors.to_vec())
    }
}

fn variable_declaration(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let val_tok = lexer.next();
    let val_tok_loc = val_tok.source_location();
    if val_tok.token_type != TokenType::Val {
        let location = val_tok_loc;

        let error_val = ParseError::new("variable declarations should start with 'val'", location);
        errors.push(error_val);
        return AstNode::Error(location);
    }

    let id_tok = lexer.peek();
    let variable_name = match id_tok.clone().token_type {
        TokenType::Identifier(name) => name,
        _ => {
            let location = lexer.previous().source_location();
            let error_val = ParseError::new("expected identifier after val", location);
            errors.push(error_val);
            return AstNode::Error(location);
        }
    };
    lexer.next();

    let eq_tok = lexer.peek();
    if eq_tok.token_type != TokenType::Equal {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new(&format!("'=' expected after {variable_name}"), location);
        errors.push(error_val);
        return AstNode::Error(location);
    }
    lexer.next();

    if [TokenType::Eof, TokenType::Val, TokenType::Semicolon].contains(&lexer.peek().token_type) {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new("Expect expression after =", location);
        errors.push(error_val);
        return AstNode::Error(location);
    }

    let assigned_expr = expression(lexer, errors);

    let semicolon_tok = lexer.peek();
    if semicolon_tok.token_type != TokenType::Semicolon {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new("expected ';' after variable declaration", location);
        errors.push(error_val);
        return AstNode::Error(location);
    }
    let semicolon_loc = lexer.next().source_location();
    let location = SourceLocation::new(val_tok_loc.start, semicolon_loc.end);
    AstNode::VariableDeclaration {
        variable: variable_name,
        value: Box::new(assigned_expr),
        location,
    }
}

fn if_expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let if_tok = lexer.peek();
    let if_loc = if_tok.source_location();
    if if_tok.token_type != TokenType::If {
        let start = if_loc.start;
        let end = if_loc.end;
        let error_val = ParseError::new("expected token if", lexer.previous().source_location());
        errors.push(error_val);
        return AstNode::Error(SourceLocation::new(start, end));
    }
    lexer.next();
    let start_cond = lexer.peek().source_location().start;
    let condition = expression(lexer, errors);
    let end_cond = lexer.peek().source_location().end;
    if let AstNode::Error(_) = condition {
        let end = end_cond;
        let error_val = ParseError::new(
            "expected expression after if",
            lexer.previous().source_location(),
        );
        errors.push(error_val);
        return AstNode::Error(SourceLocation::new(start_cond, end));
    }

    let then_tok = lexer.peek();
    let then_loc = then_tok.source_location();
    if then_tok.token_type != TokenType::Then {
        let error_val = ParseError::new("expected token then", lexer.previous().source_location());
        errors.push(error_val);
        return AstNode::Error(then_loc);
    }

    let if_body_start = lexer.next().source_location().start;
    let if_body = expression(lexer, errors);
    if let AstNode::Error(_) = if_body {
        let if_body_end = lexer.peek().source_location().end;
        let error_val = ParseError::new(
            "expected expression after then",
            lexer.previous().source_location(),
        );
        errors.push(error_val);
        return AstNode::Error(SourceLocation::new(if_body_start, if_body_end));
    }

    let else_tok = lexer.peek();
    let else_loc = else_tok.source_location();
    if else_tok.token_type != TokenType::Else {
        let error_val = ParseError::new("expected token else", lexer.previous().source_location());
        errors.push(error_val);
        return AstNode::Error(else_loc);
    }

    let else_body_loc = lexer.next().source_location();
    let else_body = expression(lexer, errors);
    let else_body_start = else_body_loc.start;
    let else_body_end = else_body_loc.end;
    if let AstNode::Error(_) = else_body {
        let error_val = ParseError::new(
            "expected expression after else",
            lexer.previous().source_location(),
        );
        errors.push(error_val);
        return AstNode::Error(SourceLocation::new(else_body_start, else_body_end));
    }

    AstNode::If {
        condition: Box::new(condition),
        if_body: Box::new(if_body),
        else_body: Box::new(else_body),
        location: SourceLocation::new(if_loc.start, lexer.peek().source_location().end),
    }
}

fn equality(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let start = lexer.peek().source_location().start;
    let lhs = term(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.next();
            let rhs = equality(lexer, errors);
            let end = lexer.previous().source_location().end;

            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn term(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let start = lexer.peek().source_location().start;
    let lhs = factor(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.next();
            let rhs = term(lexer, errors);
            let end = lexer.previous().source_location().end;

            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn factor(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let start = lexer.peek().source_location().start;
    let lhs = unary(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            lexer.next();
            let rhs = factor(lexer, errors);
            let end = lexer.previous().source_location().end;
            AstNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn unary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let token = lexer.peek();
    let start = token.source_location().start;
    match token.token_type {
        TokenType::Negation => {
            lexer.next();
            let expr = unary(lexer, errors);
            let end = lexer.previous().source_location().end;

            AstNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(expr),
                location: SourceLocation::new(start, end),
            }
        }
        _ => primary(lexer, errors),
    }
}

fn primary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> AstNode {
    let token = lexer.next();
    let tok_location = token.source_location();
    match token.token_type {
        TokenType::Int(n) => AstNode::Int(n, tok_location),
        TokenType::Float(n) => AstNode::Float(n, tok_location),
        TokenType::Bool(b) => AstNode::Bool(b, tok_location),
        TokenType::Identifier(i) => AstNode::Identifier(i, tok_location),
        TokenType::LeftParen => {
            let start = tok_location.start;
            let expr = expression(lexer, errors);
            let closing_paren = lexer.peek();
            let end = closing_paren.source_location().end;

            if closing_paren.token_type == TokenType::RightParen {
                lexer.next();
                AstNode::Grouping(Box::new(expr), SourceLocation::new(start, end))
            } else {
                let error = ParseError::new(
                    "Expected closing parenthesis",
                    lexer.peek().source_location(),
                );
                errors.push(error);
                AstNode::Error(SourceLocation::new(start, end))
            }
        }
        _ => {
            let literal = token.literal;
            let error = ParseError::new(
                &format!("unexpected token {literal}"),
                lexer.previous().source_location(),
            );
            errors.push(error);
            AstNode::Error(tok_location)
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
