use std::collections::HashMap;

use crate::{
    ast::AnnotatedASTNode,
    pattern::{to_var, PatternMatrix},
};

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

pub fn resolve_symbols(
    ast: &mut AnnotatedASTNode,
    symbol_table: &mut SymbolTable<AnnotatedASTNode>,
) {
    match ast {
        AnnotatedASTNode::Declarations(nodes, _, _) => {
            for node in nodes {
                resolve_symbols(node, symbol_table)
            }
        }
        AnnotatedASTNode::Let {
            declarations,
            expr: _,
            location: _,
            ref mut environment,
            node_id: _,
        } => {
            *environment = Some(Box::new(symbol_table.enter_scope()));
            for declaration in declarations {
                resolve_symbols(declaration, environment.as_mut().unwrap());
            }
        }
        AnnotatedASTNode::VariableDeclaration {
            pattern,
            value,
            type_declaration: _,
            location: _,
            node_id: _,
        } => {
            let annotated_astnode = *value.clone();
            let matrix = PatternMatrix::new(vec![pattern.clone()], &annotated_astnode);
            let act = matrix.action();
            for (pat, node_id) in act.bindings {
                let name = to_var(pat);
                let entry = value.lookup(node_id).unwrap();
                let node = resolve_node(entry, symbol_table);
                symbol_table.insert(&name, node);
            }
            resolve_symbols(value, symbol_table);
        }
        AnnotatedASTNode::If {
            condition,
            if_body,
            else_body,
            location: _,
            node_id: _,
        } => {
            resolve_symbols(condition, symbol_table);
            resolve_symbols(if_body, symbol_table);
            resolve_symbols(else_body, symbol_table);
        }
        AnnotatedASTNode::Binary {
            op: _,
            lhs,
            rhs,
            location: _,
            node_id: _,
        } => {
            resolve_symbols(lhs, symbol_table);
            resolve_symbols(rhs, symbol_table);
        }
        AnnotatedASTNode::Unary {
            op: _,
            expr,
            location: _,
            node_id: _,
        } => {
            resolve_symbols(expr, symbol_table);
        }
        AnnotatedASTNode::Grouping(Some(node), _, _) => resolve_symbols(node, symbol_table),
        _ => {}
    }
}

fn resolve_node(
    node: AnnotatedASTNode,
    symbol_table: &mut SymbolTable<AnnotatedASTNode>,
) -> AnnotatedASTNode {
    match node {
        AnnotatedASTNode::Tuple(decs, loc, id) => {
            let new_decs: Vec<AnnotatedASTNode> = decs
                .iter()
                .map(|dec| resolve_node(dec.clone(), symbol_table))
                .collect();

            AnnotatedASTNode::Tuple(new_decs, loc, id)
        }
        AnnotatedASTNode::Identifier(s, loc, id) => {
            if let Some(entry) = symbol_table.lookup(&s) {
                entry
            } else {
                AnnotatedASTNode::Identifier(s, loc, id)
            }
        }
        n => n,
    }
}
