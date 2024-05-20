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

pub fn stringify_parse_errors(errors: ParseErrorList) -> Vec<String> {
    let mut messages: Vec<String> = Vec::new();
    for error in errors {
        messages.push(error.to_string());
    }
    messages
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let position = self.token.offset;
        write!(f, "{position} {}", self.message)
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<AstNode, Vec<ParseError>> {
    //      parse_expr(lexer, 0)
    let mut errors: Vec<ParseError> = Vec::new();
    declarations(lexer, &mut errors)
}

fn recover_from_error(lexer: &mut Lexer) {
    println!("Starting error recovery at {}", lexer.peek().clone());
    while lexer.peek().token_type != TokenType::Eof {
        println!("{}", lexer.peek());
        match lexer.peek().token_type {
            TokenType::Val => {
                println!("FOUND VAL, Recovered at  {}", lexer.peek().clone());
                break;
            }
            _ => {
                println!("Skipping {:?}", lexer.peek().clone());
                lexer.next();
            }
        }
    }
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
                match var_declaration {
                    Ok(node) => {
                        declarations.push(node);
                    }
                    Err(_) => {
                        if lexer.peek().token_type != TokenType::Eof {
                            recover_from_error(lexer);
                            found_error = true;
                        }
                    }
                }
            }
            _ => {
                let expr = expression(lexer, errors);
                match expr {
                    Ok(node) => {
                        declarations.push(node);
                    }
                    Err(_) => {
                        println!("{lexer:#?}");
                        if lexer.peek().token_type != TokenType::Eof {
                            recover_from_error(lexer);
                            found_error = true;
                        }
                    }
                }
            }
        }
    }
    println!("{declarations:#?}");
    println!("{errors:#?}");
    if found_error {
        Err(errors.to_vec())
    } else {
        Ok(AstNode::Declarations(declarations))
    }
}

fn variable_declaration(
    lexer: &mut Lexer,
    errors: &mut Vec<ParseError>,
) -> Result<AstNode, ParseErrorList> {
    let val_tok = lexer.next();
    if val_tok.token_type != TokenType::Val {
        println!("{}", val_tok);
        let error_val = ParseError::new("variable declarations should start with 'val'", val_tok);
        errors.push(error_val);
        return Err(errors.clone());
    }

    let id_tok = lexer.next();
    let variable_name = match id_tok.clone().token_type {
        TokenType::Identifier(name) => name,
        _ => {
            let error_val = ParseError::new("expected identifier after val", id_tok);
            errors.push(error_val);
            return Err(errors.clone());
        }
    };

    let eq_tok = lexer.next();
    if eq_tok.token_type != TokenType::Equal {
        println!("{}", val_tok);
        let error_val = ParseError::new(&format!("'=' expected after {variable_name}"), eq_tok);
        errors.push(error_val);
        return Err(errors.clone());
    }

    if lexer.peek().token_type == TokenType::Eof {
        let error_val = ParseError::new("Expect expression after =", lexer.peek().clone());
        errors.push(error_val);
        return Err(errors.clone());
    }
    let assigned_expr = expression(lexer, errors);

    let semicolon_tok = lexer.peek();
    if semicolon_tok.token_type != TokenType::Semicolon {
        let error_val =
            ParseError::new("expected ';' after variable declaration", lexer.previous());
        errors.push(error_val);
        return Err(errors.clone());
    }
    lexer.next();

    println!("{id_tok} {eq_tok} {assigned_expr:?}");
    match assigned_expr {
        Ok(expr) => Ok(AstNode::VariableDeclaration {
            variable: variable_name,
            value: Box::new(expr),
        }),
        err => err,
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
        _ => {
            let error = ParseError::new("expected a literal value", lexer.peek());
            errors.push(error);
            Err(errors.clone())
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
