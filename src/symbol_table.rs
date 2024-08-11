use std::collections::HashMap;

use crate::{ast::ASTNode, error_reporting::CompilerError, source_location::SourceLocation};

#[derive(Debug, Clone)]
pub struct SymbolTable<T> {
    table: HashMap<String, T>,
    outer: Option<Box<SymbolTable<T>>>,
}

impl<T> Default for SymbolTable<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SymbolTable<T>
where
    T: Clone,
{
    pub fn new() -> SymbolTable<T> {
        let table: HashMap<String, T> = HashMap::new();
        SymbolTable { table, outer: None }
    }

    pub fn enter_scope(&self) -> SymbolTable<T> {
        let table: HashMap<String, T> = HashMap::new();
        let symbol_table = Box::new(self.clone());
        SymbolTable {
            table,
            outer: Some(symbol_table),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<T> {
        match self.table.get(name) {
            Some(v) => Some(v.clone()),
            None => {
                if let Some(outer) = &self.outer {
                    outer.lookup(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn insert(&mut self, name: &str, entry: T) {
        self.table.insert(name.to_string(), entry);
    }
}

pub type NameErrorList = Vec<NameError>;
#[derive(Debug)]
pub struct NameError {
    message: String,
    location: SourceLocation,
    details: Option<Vec<(String, SourceLocation)>>,
    explaination: Option<String>,
}
impl CompilerError for NameError {
    fn new(
        message: &str,
        location: SourceLocation,
        details: Option<Vec<(String, SourceLocation)>>,
        explaination: Option<String>,
    ) -> NameError {
        NameError {
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
        "NameError"
    }
    fn details(&self) -> Option<Vec<(String, SourceLocation)>> {
        self.details.clone()
    }
    fn explaination(&self) -> Option<String> {
        self.explaination.clone()
    }
}

pub fn resolve_symbols(ast: &mut ASTNode) -> Result<SymbolTable<ASTNode>, NameErrorList> {
    let mut symbol_table: SymbolTable<ASTNode> = SymbolTable::new();
    let mut errors: NameErrorList = Vec::new();
    resolve_node(ast, &mut symbol_table, &mut errors);
    if errors.is_empty() {
        Ok(symbol_table)
    } else {
        Err(errors)
    }
}
pub fn resolve_node(
    ast: &mut ASTNode,
    symbol_table: &mut SymbolTable<ASTNode>,
    errors: &mut NameErrorList,
) {
    match ast {
        ASTNode::Declarations(nodes, _) => {
            for node in nodes {
                resolve_node(node, symbol_table, errors);
            }
        }
        ASTNode::Let {
            declarations,
            expr: _,
            location: _,
            ref mut environment,
        } => {
            *environment = Some(Box::new(symbol_table.enter_scope()));
            for declaration in declarations {
                resolve_node(declaration, environment.as_mut().unwrap(), errors);
            }
        }
        ASTNode::VariableDeclaration {
            variable,
            value,
            type_declaration: _,
            location: _,
        } => {
            symbol_table.insert(variable, *value.to_owned());
            resolve_node(value, symbol_table, errors);
        }
        ASTNode::If {
            condition,
            if_body,
            else_body,
            location: _,
        } => {
            resolve_node(condition, symbol_table, errors);
            resolve_node(if_body, symbol_table, errors);
            resolve_node(else_body, symbol_table, errors);
        }
        ASTNode::Binary {
            op: _,
            lhs,
            rhs,
            location: _,
        } => {
            resolve_node(lhs, symbol_table, errors);
            resolve_node(rhs, symbol_table, errors);
        }
        ASTNode::Unary {
            op: _,
            expr,
            location: _,
        } => {
            resolve_node(expr, symbol_table, errors);
        }
        ASTNode::Grouping(Some(node), _) => resolve_node(node, symbol_table, errors),
        ASTNode::Identifier(variable, location) => {
            if symbol_table.lookup(variable).is_none() {
                let err = NameError::new(
                    &format!("Undefined variable {}", variable),
                    *location,
                    None,
                    None,
                );
                errors.push(err);
            }
        }
        ASTNode::Tuple(nodes, _) => {
            for node in nodes {
                resolve_node(node, symbol_table, errors);
            }
        }

        _ => {}
    }
}
