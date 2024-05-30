use crate::ast::{AnnotatedASTNode, Pattern};

type Binding = (Pattern, usize);
type Clause = (Vec<Pattern>, Action);

#[derive(Debug, Clone)]
pub struct Action {
    pub bindings: Vec<Binding>,
}

#[derive(Debug, Clone)]
pub struct PatternMatrix {
    clauses: Vec<Clause>,
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
        let mut clauses: Vec<Clause> = vec![];
        for pattern in patterns {
            let row = match pattern {
                Pattern::Wildcard(_) => vec![pattern.clone()],
                Pattern::Variable(_, _) => vec![pattern.clone()],
                Pattern::Int(_, _) => vec![pattern.clone()],
                Pattern::Float(_, _) => vec![pattern.clone()],
                Pattern::Tuple(inner, _) => inner.clone(),
                Pattern::Error(_) => todo!(),
            };
            let bindings: Vec<Binding> = if row.len() == 1 {
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
    pub fn default(&self, column: usize) -> PatternMatrix {
        let mut clauses: Vec<Clause> = vec![];
        for row in &self.clauses {
            let (pats, _) = row.clone();
            if irrefutable(&pats[column]) {
                clauses.push(row.clone());
            }
        }

        PatternMatrix {
            clauses,
            occurences: self.occurences.clone(),
        }
    }
    pub fn swap(&mut self, a: usize, b: usize) {
        let (top, children) = &mut self.occurences;
        children.swap(a, b);
        for row in &mut self.clauses {
            let (pats, _) = row;
            pats.swap(a, b);
            
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
fn irrefutable(pat: &Pattern) -> bool {
    match pat {
        Pattern::Wildcard(_) => true,
        Pattern::Variable(_, _) => true,
        Pattern::Tuple(pats, _) => {
            for pat in pats {
                if irrefutable(pat) {
                    return true;
                }
            }
            return false;
        }
        _ => false,
    }
}
