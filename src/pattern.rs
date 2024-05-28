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
pub struct Action {
    bindings: Vec<(Pattern, usize)>,
}
#[derive(Debug)]
pub struct PatternMatrix {
    clauses: Vec<(Vec<Pattern>, Action)>,
    occurences: (usize, Vec<usize>),
}

impl PatternMatrix {
    pub fn from_ast(node: &AnnotatedASTNode) -> PatternMatrix {
        match node {
            AnnotatedASTNode::VariableDeclaration {
                pattern,
                type_declaration,
                value,
                location,
                node_id,
            } => {
                let top_level = value.node_id();
                let os = occurences(value);

                let row = match pattern {
                    Pattern::Wildcard(_) => vec![pattern.clone()],
                    Pattern::Variable(_, _) => vec![pattern.clone()],
                    Pattern::Int(_, _) => vec![pattern.clone()],
                    Pattern::Float(_, _) => vec![pattern.clone()],
                    Pattern::Tuple(inner, _) => inner.clone(),
                    Pattern::Error(_) => todo!(),
                };
                let bindings: Vec<(Pattern, usize)> = if row.len() == 1 {
                    vec![(row[0].clone(), top_level)]
                } else {
                    row.iter()
                        .zip(os.clone())
                        .map(|(pat, size)| (pat.clone(), size))
                        .collect()
                };

                let action = Action { bindings };
                let clauses = vec![(row.clone(), action)];
                PatternMatrix {
                    clauses,
                    occurences: (top_level, os),
                }
            }
            _ => todo!(),
        }
    }
}

fn occurences(node: &AnnotatedASTNode) -> Vec<usize> {
    match node {
        AnnotatedASTNode::Tuple(nodes, _, _) => nodes.iter().map(|x| x.node_id()).collect(),
        AnnotatedASTNode::Error(_, _) => panic!("Cannot work with error nodes"),
        others => vec![others.node_id()],
    }
}
fn matches_all(pat: Pattern) -> bool {
    match pat {
        Pattern::Wildcard(_) => true,
        Pattern::Variable(_, _) => true,
        _ => false,
    }
}
