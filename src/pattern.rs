use std::{collections::HashMap, os::fd::AsFd};

use crate::{
    ast::{ASTNode, AnnotatedASTNode, Pattern},
    error_reporting::CompilerError,
    lexer::Lexer,
    parser::ParseError,
    source_location::{SourceLocation, SourcePosition},
    token::TokenType,
};

#[derive(Debug)]
pub struct PatternMatrix {
    occurences: Vec<usize>,
    rows: Vec<Row>,
}

impl PatternMatrix {
    pub fn from_ast(ast: &AnnotatedASTNode) -> PatternMatrix {
        match ast {
            AnnotatedASTNode::VariableDeclaration {
                pattern,
                type_declaration,
                value,
                location,
                node_id,
            } => {
                let row = Row::from_ast(ast);
                let occurences = [1, 2].to_vec();
                PatternMatrix {
                    rows: vec![row],
                    occurences,
                }
            },
            _ => panic!("Match not supported")
        }
    }
}

fn split_pattern(pattern: Pattern, occurences: Vec<usize>) -> Vec<Pattern> {
    match pattern {
        Pattern::Tuple(pats, _) => pats,
        Pattern::Float(_, _)
        | Pattern::Int(_, _)
        | Pattern::Variable(_, _)
        | Pattern::Wildcard(_) => {
            vec![pattern]
        }
        Pattern::Error(_) => panic!("DO NOT USE ERROR PATTERN"),
    }
}

fn occurrences(node: AnnotatedASTNode) -> Vec<usize> {
    match node {
        AnnotatedASTNode::Tuple(items, _, _) => {
            let mut ocs: Vec<usize> = vec![];
            for i in items {
                for o in occurrences(i) {
                    ocs.push(o);
                }
            }
            ocs
        }
        v => vec![v.node_id()],
    }
}
#[derive(Debug)]
struct Row {
    columns: Vec<Column>,
    action: Action,
}

impl Row {
    fn new(columns: Vec<Column>, action: Action) -> Row {
        Row { columns, action }
    }

    fn from_ast(ast: &AnnotatedASTNode) -> Row {
        match ast {
            AnnotatedASTNode::VariableDeclaration {
                pattern,
                type_declaration: _,
                ref value,
                location,
                node_id,
            } => {
                todo!()
            }
            _ => panic!("Panic pattern matching not support here"),
        }
    }
}
#[derive(Debug)]
struct Column {
    id: usize,
    pattern: Pattern,
}

impl Column {
    fn new(id: usize, pattern: Pattern) -> Column {
        Column { id, pattern }
    }
    fn binding(&self, node: &AnnotatedASTNode) -> (usize, AnnotatedASTNode) {
        let target = node.lookup(self.id).unwrap();
        (self.id, target)
    }
}

#[derive(Debug)]
struct Action {
    bindings: Vec<(usize, AnnotatedASTNode)>,
    expr: Option<Box<AnnotatedASTNode>>,
}

impl Action {
    fn new(bindings: Vec<(usize, AnnotatedASTNode)>) -> Action {
        Action {
            bindings,
            expr: None,
        }
    }
}
