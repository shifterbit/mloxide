use crate::ast::{ASTNode, Operator};
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
    details: Option<Vec<(String, SourceLocation)>>,
    explaination: Option<String>,
}

impl CompilerError for ParseError {
    fn new(
        message: &str,
        location: SourceLocation,
        details: Option<Vec<(String, SourceLocation)>>,
        explaination: Option<String>,
    ) -> ParseError {
        ParseError {
            message: message.to_string(),
            location,
            details,
            explaination,
        }
    }
    fn location(&self) -> SourceLocation {
        self.location
    }
    fn message(&self) -> &str {
        &self.message
    }
    fn error_type(&self) -> &str {
        "SyntaxError"
    }
    fn details(&self) -> Option<Vec<(String, SourceLocation)>> {
        self.details.clone()
    }
    fn explaination(&self) -> Option<String> {
        self.explaination.clone()
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<ASTNode, (ParseErrorList, ASTNode)> {
    let mut errors: Vec<ParseError> = Vec::new();
    program(lexer, &mut errors)
}

fn recover_from_error(lexer: &mut Lexer) {
    while lexer.peek().token_type != TokenType::Eof {
        match lexer.peek().token_type {
            TokenType::Val => {
                break;
            }
            _ => {
                lexer.consume();
            }
        }
    }
}

fn expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let curr = lexer.peek();
    let token_type = curr.token_type.clone();
    let start = curr.source_location().start;

    match token_type {
        TokenType::If => if_expression(lexer, errors),
        TokenType::Eof => {
            let location = SourceLocation::new(start, curr.source_location().end);
            ASTNode::Error(location)
        }
        TokenType::Let => let_expression(lexer, errors),
        _ => equality(lexer, errors),
    }
}

fn program(
    lexer: &mut Lexer,
    errors: &mut Vec<ParseError>,
) -> Result<ASTNode, (ParseErrorList, ASTNode)> {
    let mut declarations: Vec<ASTNode> = Vec::new();
    let declarations_loc_start = lexer.peek().source_location();
    let start = declarations_loc_start.start;
    let mut end = declarations_loc_start.end;
    while lexer.peek().token_type != TokenType::Eof {
        match lexer.peek().token_type {
            TokenType::Val => {
                let var_declaration = variable_declaration(lexer, errors);
                declarations.push(var_declaration.clone());
                if let ASTNode::Error(_) = var_declaration {
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
    let location = SourceLocation::new(start, end);

    if errors.is_empty() {
        Ok(ASTNode::Declarations(declarations, location))
    } else {
        Err((
            errors.to_vec(),
            ASTNode::Declarations(declarations, location),
        ))
    }
}

fn variable_declaration(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let val_tok = lexer.consume();
    let val_tok_loc = val_tok.source_location();
    if val_tok.token_type != TokenType::Val {
        let location = val_tok_loc;

        let error_val = ParseError::new(
            "variable declarations should start with 'val'",
            location,
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(location);
    }

    let id_tok = lexer.peek();
    let variable_name = match id_tok.clone().token_type {
        TokenType::Identifier(name) => name,
        _ => {
            let location = lexer.previous().source_location();
            let error_val = ParseError::new("expected identifier after val", location, None, None);
            errors.push(error_val);
            return ASTNode::Error(location);
        }
    };
    lexer.consume();

    let eq_tok = lexer.peek();
    if eq_tok.token_type != TokenType::Equal {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new(
            &format!("'=' expected after {variable_name}"),
            location,
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(location);
    }
    lexer.consume();

    if [TokenType::Eof, TokenType::Val, TokenType::Semicolon].contains(&lexer.peek().token_type) {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new("Expect expression after =", location, None, None);
        errors.push(error_val);
        return ASTNode::Error(location);
    }

    let assigned_expr = expression(lexer, errors);

    let semicolon_tok = lexer.peek();
    if semicolon_tok.token_type != TokenType::Semicolon {
        let location = lexer.previous().source_location();
        let error_val = ParseError::new(
            "expected ';' after variable declaration",
            location,
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(location);
    }
    let semicolon_loc = lexer.consume().source_location();
    let location = SourceLocation::new(val_tok_loc.start, semicolon_loc.end);
    ASTNode::VariableDeclaration {
        variable: variable_name,
        value: Box::new(assigned_expr),
        location,
    }
}

fn let_expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let let_tok = lexer.peek();
    let let_loc = let_tok.source_location();
    if let_tok.token_type != TokenType::Let {
        let start = let_loc.start;
        let end = let_loc.end;
        let error_val = ParseError::new(
            "expected token let",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(start, end));
    }
    lexer.consume();
    let mut declarations: Vec<ASTNode> = Vec::new();
    while [TokenType::Val].contains(&lexer.peek().token_type) {
        let declaration = variable_declaration(lexer, errors);
        declarations.push(declaration);
    }
    let in_tok = lexer.peek();
    let in_loc = in_tok.source_location();
    if in_tok.token_type != TokenType::In {
        let start = in_loc.start;
        let end = in_loc.end;
        let error_val = ParseError::new(
            "expected token in after variable declaration",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(start, end));
    }
    lexer.consume();
    let expr = expression(lexer, errors);

    let end_tok = lexer.consume();
    let end_loc = end_tok.source_location();
    if end_tok.token_type != TokenType::End {
        let _start = end_loc.start;
        let end = end_loc.end;
        let error_val = ParseError::new(
            "expected token end",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(let_loc.start, end));
    }

    ASTNode::Let {
        declarations,
        expr: Box::new(expr),
        location: SourceLocation::new(let_loc.start, end_loc.end),
        environment: None,
    }
}
fn if_expression(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let if_tok = lexer.peek();
    let if_loc = if_tok.source_location();
    if if_tok.token_type != TokenType::If {
        let start = if_loc.start;
        let end = if_loc.end;
        let error_val = ParseError::new(
            "expected token if",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(start, end));
    }
    lexer.consume();
    let start_cond = lexer.peek().source_location().start;
    let condition = expression(lexer, errors);
    let end_cond = lexer.peek().source_location().end;
    if let ASTNode::Error(_) = condition {
        let end = end_cond;
        let error_val = ParseError::new(
            "expected expression after if",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(start_cond, end));
    }

    let then_tok = lexer.peek();
    let then_loc = then_tok.source_location();
    if then_tok.token_type != TokenType::Then {
        let error_val = ParseError::new(
            "expected token then",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(then_loc);
    }

    let if_body_start = lexer.consume().source_location().start;
    let if_body = expression(lexer, errors);
    if let ASTNode::Error(_) = if_body {
        let if_body_end = lexer.peek().source_location().end;
        let error_val = ParseError::new(
            "expected expression after then",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(if_body_start, if_body_end));
    }

    let else_tok = lexer.peek();
    let else_loc = else_tok.source_location();
    if else_tok.token_type != TokenType::Else {
        let error_val = ParseError::new(
            "expected token else",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(else_loc);
    }

    let else_body_loc = lexer.consume().source_location();
    let else_body = expression(lexer, errors);
    let else_body_start = else_body_loc.start;
    let else_body_end = else_body_loc.end;
    if let ASTNode::Error(_) = else_body {
        let error_val = ParseError::new(
            "expected expression after else",
            lexer.previous().source_location(),
            None,
            None,
        );
        errors.push(error_val);
        return ASTNode::Error(SourceLocation::new(else_body_start, else_body_end));
    }

    ASTNode::If {
        condition: Box::new(condition),
        if_body: Box::new(if_body),
        else_body: Box::new(else_body),
        location: SourceLocation::new(if_loc.start, lexer.peek().source_location().end),
    }
}

fn equality(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let start = lexer.peek().source_location().start;
    let lhs = comparison(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::EqualEqual | TokenType::NotEqual => {
            lexer.consume();
            let rhs = equality(lexer, errors);
            let end = lexer.previous().source_location().end;

            ASTNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn comparison(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let start = lexer.peek().source_location().start;
    let lhs = term(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::LeftArrow
        | TokenType::LeftArrowEqual
        | TokenType::RightArrowEqual
        | TokenType::RightArrow => {
            lexer.consume();
            let rhs = comparison(lexer, errors);
            let end = lexer.previous().source_location().end;

            ASTNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}
fn term(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let start = lexer.peek().source_location().start;
    let lhs = factor(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::Minus | TokenType::Plus => {
            lexer.consume();
            let rhs = term(lexer, errors);
            let end = lexer.previous().source_location().end;

            ASTNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn factor(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let start = lexer.peek().source_location().start;
    let lhs = unary(lexer, errors);

    let token = lexer.peek();
    match token.token_type {
        TokenType::ForwardSlash | TokenType::Star => {
            lexer.consume();
            let rhs = factor(lexer, errors);
            let end = lexer.previous().source_location().end;
            ASTNode::Binary {
                op: get_operator(token.token_type),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                location: SourceLocation::new(start, end),
            }
        }
        _ => lhs,
    }
}

fn unary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let token = lexer.peek();
    let start = token.source_location().start;
    match token.token_type {
        TokenType::Negation => {
            lexer.consume();
            let expr = unary(lexer, errors);
            let end = lexer.previous().source_location().end;

            ASTNode::Unary {
                op: get_operator(token.token_type),
                expr: Box::new(expr),
                location: SourceLocation::new(start, end),
            }
        }
        _ => primary(lexer, errors),
    }
}

fn primary(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> ASTNode {
    let token = lexer.consume();
    let tok_location = token.source_location();
    match token.token_type {
        TokenType::Int(n) => ASTNode::Int(n, tok_location),
        TokenType::Float(n) => ASTNode::Float(n, tok_location),
        TokenType::Bool(b) => ASTNode::Bool(b, tok_location),
        TokenType::Identifier(i) => ASTNode::Identifier(i, tok_location),
        TokenType::LeftParen => {
            let start = tok_location.start;
            if lexer.peek().token_type == TokenType::RightParen {
                let closing = lexer.consume();
                return ASTNode::Grouping(
                    None,
                    SourceLocation::new(start, closing.source_location().end),
                );
            }
            let expr = expression(lexer, errors);
            let closing_paren = lexer.peek();
            let end = closing_paren.source_location().end;

            if closing_paren.token_type == TokenType::RightParen {
                lexer.consume();
                ASTNode::Grouping(Some(Box::new(expr)), SourceLocation::new(start, end))
            } else {
                let error = ParseError::new(
                    "Expected closing parenthesis",
                    lexer.peek().source_location(),
                    None,
                    None,
                );
                errors.push(error);
                ASTNode::Error(SourceLocation::new(start, end))
            }
        }
        _ => {
            let literal = token.literal;
            let error = ParseError::new(
                &format!("unexpected token {literal}"),
                lexer.previous().source_location(),
                None,
                None,
            );
            errors.push(error);
            ASTNode::Error(tok_location)
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
        TokenType::RightArrow => Operator::GreaterThan,
        TokenType::LeftArrow => Operator::LessThan,
        TokenType::RightArrowEqual => Operator::GreaterEqual,
        TokenType::LeftArrowEqual => Operator::LessEqual,
        _ => panic!("Invalid Operator"),
    }
}
