use std::{
    collections::HashMap,
};

use crate::ast::ASTNode;

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

pub fn resolve_symbols(ast: &mut ASTNode, symbol_table: &mut SymbolTable<ASTNode>) {
    match ast {
        ASTNode::Declarations(nodes, _) => {
            for node in nodes {
                resolve_symbols(node, symbol_table)
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
                resolve_symbols(declaration, environment.as_mut().unwrap());
            }
        }
        ASTNode::VariableDeclaration {
            variable,
            value,
            location: _,
        } => {
            symbol_table.insert(variable, *value.to_owned());
            resolve_symbols(value, symbol_table);
        }
        ASTNode::If {
            condition,
            if_body,
            else_body,
            location: _,
        } => {
            resolve_symbols(condition, symbol_table);
            resolve_symbols(if_body, symbol_table);
            resolve_symbols(else_body, symbol_table);
        }
        ASTNode::Binary {
            op: _,
            lhs,
            rhs,
            location: _,
        } => {
            resolve_symbols(lhs, symbol_table);
            resolve_symbols(rhs, symbol_table);
        }
        ASTNode::Unary {
            op: _,
            expr,
            location: _,
        } => {
            resolve_symbols(expr, symbol_table);
        }
        ASTNode::Grouping(Some(node), _) => resolve_symbols(node, symbol_table),
        _ => {}
    }
}
