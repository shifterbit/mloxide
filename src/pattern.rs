use std::collections::HashMap;

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

impl Default for PatternMatrix {
    fn default() -> Self {
        Self {
            clauses: Default::default(),
            occurences: Default::default(),
        }
    }
}
#[derive(Eq, Hash, PartialEq)]
pub enum PatternKind {
    Tuple,
    Int,
    Float,
    Wildcard,
    Variable,
    Error,
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
    pub fn single_column(&self, column: usize) -> Vec<(Pattern, Action)> {
        let mut cols: Vec<(Pattern, Action)> = vec![];

        for clause in &self.clauses {
            let (row, act) = clause;
            let entry = (row.clone()[column].clone(), act.clone());
            cols.push(entry);
        }
        cols
    }

    pub fn from_ast(patterns: Vec<Pattern>, target: &AnnotatedASTNode) -> PatternMatrix {
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
    pub fn specialize_default(&self) -> PatternMatrix {
        let mut clauses: Vec<Clause> = vec![];
        for row in &self.clauses {
            let (pats, _) = row.clone();
            if pats[0].contains_irrefutable() {
                clauses.push(row.clone());
            }
        }

        PatternMatrix {
            clauses,
            occurences: self.occurences.clone(),
        }
    }
    pub fn specialize(&self, clauses: Vec<Clause>, kind: PatternKind) -> PatternMatrix {
        let (_, children) = self.occurences.clone();
        println!("{:?}", children);
        let top_level = children[0];
        let mut occurence_vector: Vec<usize> = vec![];
        let mut specialized_clauses: Vec<Clause> = vec![];
        for clause in clauses {
            match kind {
                PatternKind::Tuple => {
                    let (pats, act) = clause;
                    if let Pattern::Tuple(decs, _) = pats[0].clone() {
                        println!("{:?}", decs);
                        for i in 1..(decs.len() + 1) {
                            occurence_vector.push(top_level + i);
                        }

                        let (_, rest) = act.bindings.split_at(1);
                        let mut bindings = vec![];
                        for i in 0..decs.len() {
                            let bind: (Pattern, usize) = (decs[i].clone(), occurence_vector[i]);
                            bindings.push(bind)
                        }
                        rest.iter().for_each(|x| bindings.push(x.clone()));
                        let action = Action { bindings };
                        println!("SPECIAL {action:#?}");

                        specialized_clauses.push((decs.clone(), action));
                    }
                }
                PatternKind::Int => todo!(),
                PatternKind::Float => todo!(),
                PatternKind::Wildcard => todo!(),
                PatternKind::Variable => todo!(),
                PatternKind::Error => todo!(),
            }
        }

        let pattern_matrix = PatternMatrix {
            clauses: specialized_clauses,
            occurences: (top_level, occurence_vector),
        };
        println!("{:#?}", pattern_matrix);
        pattern_matrix
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
        let swapped_matrix = matrix.swap(col, 0);
        let first_column = swapped_matrix.single_column(0);
        let mut variants: HashMap<PatternKind, Vec<Clause>> = HashMap::new();
        for (entry, act) in first_column {
            if !entry.irrefutable() {
                let kind = entry.kind();
                if let Some(clauses) = variants.get(&kind) {
                    let mut new_clauses = clauses.clone();
                    let entries = vec![entry];
                    new_clauses.push((entries, act));
                    variants.insert(kind, new_clauses);
                } else {
                    let entries = vec![entry];
                    variants.insert(kind, vec![(entries, act)]);
                }
            }
        }
        let mut matrices: Vec<PatternMatrix> = vec![];
        for (kind, clauses) in variants {
            let specialized_matrix = matrix.specialize(clauses, kind);
            matrices.push(specialized_matrix);
        }
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
    fn kind(&self) -> PatternKind {
        match self {
            Pattern::Wildcard(_) => PatternKind::Wildcard,
            Pattern::Variable(_, _) => PatternKind::Variable,
            Pattern::Int(_, _) => PatternKind::Int,
            Pattern::Float(_, _) => PatternKind::Float,
            Pattern::Tuple(_, _) => PatternKind::Tuple,
            Pattern::Error(_) => PatternKind::Error,
        }
    }
    fn contains_irrefutable(&self) -> bool {
        match self {
            Pattern::Wildcard(_) => true,
            Pattern::Variable(_, _) => true,
            Pattern::Tuple(pats, _) => {
                for pat in pats {
                    if pat.contains_irrefutable() {
                        return true
                    }
                }
                false
            }
            _ => false,
        }
    }
}
