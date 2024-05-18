use std::{collections::HashMap, env::var};

use crate::ast::AstNode;

#[derive(Debug)]
pub struct SymbolTable {
    curr: HashMap<String, AstNode>,
    prev: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let curr: HashMap<String, AstNode> = HashMap::new();
        SymbolTable { curr, prev: None  }
    }

    pub fn new_scope(self) -> SymbolTable {
        let curr: HashMap<String, AstNode> = HashMap::new();
        SymbolTable { curr, prev: Some(Box::new(self))  }
    }

    pub fn get_variable(self, name: &str) -> Option<AstNode> {
        match self.curr.get(name) {
            Some(node) => Some(node.clone()),
            None => match self.prev {
                Some(symbol_table) => symbol_table.get_variable(name),
                None => None
            }                
        }
    }

    pub fn store_variable(&mut self, name: &str, expr: AstNode) {
        self.curr.insert(name.to_string(), expr);
    }    
}

pub fn resolve_symbols(ast: AstNode, symbol_table: &mut SymbolTable)  {
    match ast {
        AstNode::VariableDeclaration { variable, value } => {
            symbol_table.store_variable(&variable, *value);
        },
        AstNode::Int(_) | AstNode::Float(_) | AstNode::Bool(_) | AstNode::Identifier(_) => {},
        _ => {}
    }
    
}

