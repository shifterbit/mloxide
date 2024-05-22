use std::collections::HashMap;

use crate::ast::ASTNode;

#[derive(Debug, Clone)]
pub struct SymbolTable<T> {
    curr: HashMap<String, T>,
    prev: Option<Box<SymbolTable<T>>>,
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
        let curr: HashMap<String, T> = HashMap::new();
        SymbolTable { curr, prev: None }
    }

    pub fn new_scope(self) -> SymbolTable<T> {
        let curr: HashMap<String, T> = HashMap::new();
        SymbolTable {
            curr,
            prev: Some(Box::new(self)),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<T> {
        match self.curr.get(name) {
            Some(node) => Some(node.clone()),
            None => match &self.prev {
                Some(symbol_table) => symbol_table.lookup(name),
                None => None,
            },
        }
    }

    pub fn insert(&mut self, name: &str, entry: T) {
        self.curr.insert(name.to_string(), entry);
    }
}

pub fn resolve_symbols(ast: ASTNode, symbol_table: &mut SymbolTable<ASTNode>) {
    match ast {
        ASTNode::Declarations(nodes, _) => {
            for node in nodes {
                resolve_symbols(node, symbol_table)
            }
        }
        ASTNode::VariableDeclaration {
            variable,
            value,
            location: _,
        } => {
            symbol_table.insert(&variable, *value);
        }
        ASTNode::Int(_, _)
        | ASTNode::Float(_, _)
        | ASTNode::Bool(_, _)
        | ASTNode::Identifier(_, _) => {}
        _ => {}
    }
}