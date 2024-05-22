use std::{ascii::AsciiExt, collections::HashMap, fmt::format};

use crate::ast::ASTNode;

#[derive(Debug, Clone)]
pub struct SymbolTable<T> {
    curr: HashMap<String, T>,
    depth: usize,
    count: usize,
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
        SymbolTable {
            curr,
            depth: 0,
            count: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        self.depth += 1;
    }

    pub fn exit_scope(&mut self) {
        self.depth -= 1;
    }

    pub fn next_scope(&mut self) {
        self.count += 1;
    }

    pub fn lookup(&self, name: &str) -> Option<T> {
        let new_name = self.convert_to_scope_format(name);
        match self.curr.get(&new_name) {
            Some(v) => Some(v.clone()),
            None => {
                let (depth, name, count) = SymbolTable::<T>::parse_name(&new_name);
                if depth < 1 {
                    return None;
                }

                let upper_name = &format!("{}{name}{count}", (depth - 1));
                self.lookup(upper_name)
            }
        }
    }
    fn convert_to_scope_format(&self, name: &str) -> String {
        let prefix = format!("{}#", self.depth);
        let suffix = format!("@{}", self.count);
        let new_name = format!("{prefix}{name}{suffix}");
        new_name
    }

    fn parse_name(name: &str) -> (usize, String, usize) {
        let chars = name.chars();
        let mut parts = vec!["".to_string(), "".to_string(), "".to_string()];
        let mut pos = 0;
        for i in chars {
            if i == '#' {
                pos = 1;
                continue;
            }
            if i == '@' {
                pos = 2;
                continue;
            }

            parts[pos].push(i);
        }
        let name = parts[1].clone();
        let prefix = parts[0].parse::<usize>().unwrap();
        let suffix = parts[2].parse::<usize>().unwrap();
        (prefix, name, suffix)
    }
    pub fn insert(&mut self, name: &str, entry: T) {
        let new_name = self.convert_to_scope_format(name);
        self.curr.insert(new_name, entry);
    }
}


pub fn resolve_symbols(
    ast: ASTNode,
    symbol_table: &mut SymbolTable<ASTNode>,
) {
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
