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
struct Action {
    bindings: Vec<(String, ASTNode)>,
}
#[derive(Debug)]
pub struct PatternMatrix {
    occurences: Vec<String>,
    rows: Vec<Row>,
}

impl PatternMatrix {
    pub fn new(ast: AnnotatedASTNode) -> PatternMatrix {
        match ast {
            AnnotatedASTNode::VariableDeclaration {
                pattern,
                type_declaration,
                value,
                location,
                node_id
            } => {
                let occurence_vector = occurrences(pattern);
                for v in occurence_vector {
                    
                }
                panic!("{occurence_vector:?}");
                // PatternMatrix { occurences: occurence_vector, rows }},
            },
            _ => panic!("Panic pattern matching not support here"),
        }
    }
}

fn occurrences(pat: Pattern) -> Vec<String> {
    match pat {
        Pattern::Wildcard(_) => vec!["_".to_string()],
        Pattern::Float(_, _) | Pattern::Int(_, _) => vec!["_".to_string()],
        Pattern::Variable(id, _) => vec![id],
        Pattern::Tuple(patterns, _) => {
            let mut occurence_vector: Vec<String> = vec![];
            for pattern in patterns {
                for occurence in occurrences(pattern) {
                    occurence_vector.push(occurence)
                }
            }

            occurence_vector
        }
        Pattern::Error(_) => panic!("Invalid Pattern"),
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
}
#[derive(Debug)]
struct Column {
    variable: String,
    pattern: Pattern,
}

impl Column {
    fn new(variable: String, pattern: Pattern) -> Column {
        Column { variable, pattern }
    }
}
