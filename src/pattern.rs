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

#[derive(Debug)]
pub enum Decision {
    // Pattern is missing
    Fail,
    // The Pattern is Matched and the Action is performed
    Success(Action),
    // Checks if the Value fits any of the given patterns
    // 1. variable to test
    // 2. Patterns to test
    // Decision to take if it fails
    Switch(usize, Vec<Clause>, Option<Box<Decision>>),
}

impl PatternMatrix {
    pub fn action(&self) -> Action {
        let fst = self.clauses.clone();
        let vec = fst.clone();
        let pair = vec[0].clone();
        pair.1
    }
    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    pub fn first_clause(&self) -> Option<&Clause> {
        self.clauses.first()
    }

    pub fn first_refutable_column(&self) -> Option<usize> {
        let mut matrix: Vec<&Vec<Pattern>> = Vec::new();
        for clause in &self.clauses {
            let (row, _) = clause;
            matrix.push(row);
        }

        for col in 0..matrix.len() {
            for row in &matrix {
                if !row[col].irrefutable() {
                    return Some(col);
                }
            }
        }
        None
    }

    pub fn new(patterns: Vec<Pattern>, target: &AnnotatedASTNode) -> PatternMatrix {
        let top_level = target.node_id();
        let os = occurences(target);
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
            if pats[column].contains_irrefutable() {
                clauses.push(row.clone());
            }
        }

        PatternMatrix {
            clauses,
            occurences: self.occurences.clone(),
        }
    }
    pub fn specialize(&self, pat: Pattern) -> PatternMatrix {
        match pat {
            Pattern::Wildcard(_) => self.clone(),
            Pattern::Variable(_, _) => self.clone(),
            Pattern::Tuple(_, _) => {
                let mut clauses: Vec<Clause> = vec![];
                todo!()
            }
            _ => panic!("Cannot Specialize on this pattern"),
        }
    }
    pub fn swap(&self, a: usize, b: usize) -> PatternMatrix {
        let mut swapped = self.clone();
        let (_, children) = &mut swapped.occurences;
        children.swap(a, b);
        for row in &mut swapped.clauses {
            let (pats, _) = row;
            pats.swap(a, b);
        }
        swapped
    }
}

pub fn match_pattern(matrix: &PatternMatrix) -> Decision {
    if matrix.is_empty() {
        // Return Leaf Node for Failure
        return Decision::Fail;
    }

    if let Some(clause) = matrix.first_clause() {
        if irrefutable_clause(clause) {
            // Return Leaf Node For Success
            let (_, act) = clause;
            return Decision::Success(act.clone());
        }
    }

    if let Some(col) = matrix.first_refutable_column() {
        let swapped = matrix.swap(col, 0);
        todo!()
    }

    Decision::Fail
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

fn irrefutable_clause(clause: &Clause) -> bool {
    let (pats, _) = clause;
    for pat in pats {
        if !pat.irrefutable() {
            return false;
        }
    }
    true
}

impl Pattern {
    fn irrefutable(&self) -> bool {
        match self {
            Pattern::Wildcard(_) => true,
            Pattern::Variable(_, _) => true,
            _ => false,
        }
    }
    fn contains_irrefutable(&self) -> bool {
        match self {
            Pattern::Wildcard(_) => true,
            Pattern::Variable(_, _) => true,
            Pattern::Tuple(pats, _) => {
                for pat in pats {
                    if self.contains_irrefutable() {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }
}
