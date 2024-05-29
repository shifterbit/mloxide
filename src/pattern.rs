use crate::ast::{AnnotatedASTNode, Pattern};

#[derive(Debug, Clone)]
pub struct Action {
    pub bindings: Vec<(Pattern, usize)>,
}

#[derive(Debug, Clone)]
pub struct PatternMatrix {
    clauses: Vec<(Vec<Pattern>, Action)>,
    occurences: (usize, Vec<usize>),
}

impl PatternMatrix {
    pub fn action(&self) -> Action {
        let fst = self.clauses.clone();
        let vec = fst.clone();
        let pair = vec[0].clone();
        pair.1
    }
    pub fn new(patterns: Vec<Pattern>, target: &AnnotatedASTNode) -> PatternMatrix {
        let top_level = target.node_id();
        let os = occurences(&target);
        let mut clauses = vec![];
        for pattern in patterns {
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
            let clause = (row.clone(), action);
            clauses.push(clause);
        }

        PatternMatrix {
            clauses,
            occurences: (top_level, os),
        }
    }
}
pub fn to_var(pat: Pattern) -> String {
    match pat {
        Pattern::Wildcard(_) => "_".to_string(),
        Pattern::Variable(s, _) => s,
        Pattern::Int(_, _) => todo!(),
        Pattern::Float(_, _) => todo!(),
        Pattern::Tuple(_inner, _) => todo!(),
        Pattern::Error(_) => todo!(),
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
