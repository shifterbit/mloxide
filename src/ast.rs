use std::{
    collections::HashMap,
    fmt::{self, Display, Pointer},
    ops::Add,
};

use crate::{
    source_location::{SourceLocation, SourcePosition},
    symbol_table::SymbolTable,
    types::Type,
};

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(SourceLocation),
    Variable(String, SourceLocation),
    Int(i64, SourceLocation),
    Float(f64, SourceLocation),
    Tuple(Vec<Pattern>, SourceLocation),
    Error(SourceLocation),
}
#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Negate,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Subtract => write!(f, "-"),
            Operator::Negate => write!(f, "~"),
            Operator::Divide => write!(f, "/"),
            Operator::Multiply => write!(f, "*"),
            Operator::Equal => write!(f, "=="),
            Operator::NotEqual => write!(f, "!="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Error(SourceLocation),
    Int(i64, SourceLocation),
    Float(f64, SourceLocation),
    Bool(bool, SourceLocation),
    Tuple(Vec<ASTNode>, SourceLocation),
    Identifier(String, SourceLocation),
    TypeVariable(String, SourceLocation),
    Grouping(Option<Box<ASTNode>>, SourceLocation),
    Declarations(Vec<ASTNode>, SourceLocation),
    VariableDeclaration {
        pattern: Pattern,
        type_declaration: Option<Box<ASTNode>>,
        value: Box<ASTNode>,
        location: SourceLocation,
    },
    Binary {
        op: Operator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
        location: SourceLocation,
    },
    Unary {
        op: Operator,
        expr: Box<ASTNode>,
        location: SourceLocation,
    },
    If {
        condition: Box<ASTNode>,
        if_body: Box<ASTNode>,
        else_body: Box<ASTNode>,
        location: SourceLocation,
    },
    Let {
        declarations: Vec<ASTNode>,
        expr: Box<ASTNode>,
        location: SourceLocation,
        environment: Option<Box<SymbolTable<ASTNode>>>,
    },
}

impl ASTNode {
    pub fn annotate(&self) -> AnnotatedASTNode {
        let mut id = Vec::with_capacity(1);
        id.push(1);
        self.annotate_node(&mut id)
    }
    fn annotate_node(&self, node_id: &mut Vec<usize>) -> AnnotatedASTNode {
        let curr_id = node_id[0];
        match self {
            ASTNode::Error(location) => {
                node_id[0] += 1;
                AnnotatedASTNode::Error(*location, curr_id)
            }
            ASTNode::Int(v, location) => {
                node_id[0] += 1;
                AnnotatedASTNode::Int(*v, *location, curr_id)
            }
            ASTNode::Float(v, location) => {
                node_id[0] += 1;
                AnnotatedASTNode::Float(*v, *location, curr_id)
            }
            ASTNode::Bool(v, location) => {
                node_id[0] += 1;
                AnnotatedASTNode::Bool(*v, *location, curr_id)
            }
            ASTNode::Identifier(v, location) => {
                node_id[0] += 1;
                AnnotatedASTNode::Identifier(v.clone(), *location, curr_id)
            }
            ASTNode::TypeVariable(v, location) => {
                node_id[0] += 1;
                AnnotatedASTNode::TypeVariable(v.clone(), *location, curr_id)
            }
            ASTNode::Grouping(v, location) => {
                let expr = if let Some(inner) = v {
                    let annotated_inner = inner.annotate_node(node_id);
                    Some(Box::new(annotated_inner))
                } else {
                    None
                };
                node_id[0] += 1;
                AnnotatedASTNode::Grouping(expr, *location, curr_id)
            }
            ASTNode::Tuple(v, location) => {
                node_id[0] += 1;

                let items: Vec<AnnotatedASTNode> =
                    v.iter().map(|item| item.annotate_node(node_id)).collect();

                AnnotatedASTNode::Tuple(items, *location, curr_id)
            }
            ASTNode::Declarations(v, location) => {
                node_id[0] += 1;
                let items: Vec<AnnotatedASTNode> =
                    v.iter().map(|item| item.annotate_node(node_id)).collect();
                AnnotatedASTNode::Declarations(items, *location, curr_id)
            }
            ASTNode::VariableDeclaration {
                pattern,
                value,
                type_declaration,
                location,
            } => {
                node_id[0] += 1;

                let type_declaration_annotated = if let Some(node) = type_declaration {
                    Some(Box::new(node.annotate_node(node_id)))
                } else {
                    None
                };
                let value = Box::new(value.annotate_node(node_id));
                AnnotatedASTNode::VariableDeclaration {
                    pattern: pattern.clone(),
                    type_declaration: type_declaration_annotated,
                    value,
                    location: *location,
                    node_id: curr_id,
                }
            }
            ASTNode::Binary {
                op,
                lhs,
                rhs,
                location,
            } => {
                let lhs = Box::new(lhs.annotate_node(node_id));
                let rhs = Box::new(rhs.annotate_node(node_id));
                node_id[0] += 1;
                AnnotatedASTNode::Binary {
                    op: *op,
                    lhs,
                    rhs,
                    location: *location,
                    node_id: curr_id,
                }
            }
            ASTNode::Unary { op, expr, location } => {
                let expr = Box::new(expr.annotate_node(node_id));
                node_id[0] += 1;
                AnnotatedASTNode::Unary {
                    op: *op,
                    expr,
                    location: *location,
                    node_id: curr_id,
                }
            }
            ASTNode::If {
                condition,
                if_body,
                else_body,
                location,
            } => {
                let condition = Box::new(condition.annotate_node(node_id));
                let if_body = Box::new(if_body.annotate_node(node_id));
                let else_body = Box::new(else_body.annotate_node(node_id));
                node_id[0] += 1;

                AnnotatedASTNode::If {
                    condition,
                    if_body,
                    else_body,
                    location: *location,
                    node_id: curr_id,
                }
            }
            ASTNode::Let {
                declarations,
                expr,
                location,
                environment,
            } => {
                node_id[0] += 1;

                let items: Vec<AnnotatedASTNode> = declarations
                    .iter()
                    .map(|item| item.annotate_node(node_id))
                    .collect();
                let new_env: Option<Box<SymbolTable<AnnotatedASTNode>>> = if environment.is_some() {
                    let table: SymbolTable<AnnotatedASTNode> = SymbolTable::new();
                    Some(Box::new(table))
                } else {
                    None
                };
                let expr = Box::new(expr.annotate_node(node_id));

                AnnotatedASTNode::Let {
                    declarations: items,
                    expr,
                    location: *location,
                    environment: new_env,
                    node_id: curr_id,
                }
            }
        }
    }
}

impl SourcePosition for ASTNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            ASTNode::Error(location) => location.clone(),
            ASTNode::Int(_, location) => location.clone(),
            ASTNode::Float(_, location) => location.clone(),
            ASTNode::Bool(_, location) => location.clone(),
            ASTNode::Identifier(_, location) => location.clone(),
            ASTNode::TypeVariable(_, location) => location.clone(),
            ASTNode::Grouping(_, location) => location.clone(),
            ASTNode::Tuple(_, location) => location.clone(),
            ASTNode::Declarations(_, location) => location.clone(),
            ASTNode::VariableDeclaration {
                pattern: _,
                value: _,
                type_declaration: _,
                location,
            } => location.clone(),
            ASTNode::Binary {
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => location.clone(),
            ASTNode::Unary {
                op: _,
                expr: _,
                location,
            } => location.clone(),
            ASTNode::If {
                condition: _,
                if_body: _,
                else_body: _,
                location,
            } => location.clone(),
            ASTNode::Let {
                declarations: _,
                expr: _,
                location,
                environment: _,
            } => location.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum AnnotatedASTNode {
    Error(SourceLocation, usize),
    Int(i64, SourceLocation, usize),
    Float(f64, SourceLocation, usize),
    Bool(bool, SourceLocation, usize),
    Tuple(Vec<AnnotatedASTNode>, SourceLocation, usize),
    Identifier(String, SourceLocation, usize),
    TypeVariable(String, SourceLocation, usize),
    Grouping(Option<Box<AnnotatedASTNode>>, SourceLocation, usize),
    Declarations(Vec<AnnotatedASTNode>, SourceLocation, usize),
    VariableDeclaration {
        pattern: Pattern,
        type_declaration: Option<Box<AnnotatedASTNode>>,
        value: Box<AnnotatedASTNode>,
        location: SourceLocation,
        node_id: usize,
    },
    Binary {
        op: Operator,
        lhs: Box<AnnotatedASTNode>,
        rhs: Box<AnnotatedASTNode>,
        location: SourceLocation,
        node_id: usize,
    },
    Unary {
        op: Operator,
        expr: Box<AnnotatedASTNode>,
        location: SourceLocation,
        node_id: usize,
    },
    If {
        condition: Box<AnnotatedASTNode>,
        if_body: Box<AnnotatedASTNode>,
        else_body: Box<AnnotatedASTNode>,
        location: SourceLocation,
        node_id: usize,
    },
    Let {
        declarations: Vec<AnnotatedASTNode>,
        expr: Box<AnnotatedASTNode>,
        location: SourceLocation,
        environment: Option<Box<SymbolTable<AnnotatedASTNode>>>,
        node_id: usize,
    },
}

impl AnnotatedASTNode {
    pub fn lookup(&self, id: usize) -> Option<AnnotatedASTNode> {
        match self {
            AnnotatedASTNode::Error(_, _)
            | AnnotatedASTNode::Int(_, _, _)
            | AnnotatedASTNode::Float(_, _, _)
            | AnnotatedASTNode::Bool(_, _, _)
            | AnnotatedASTNode::Identifier(_, _, _)
            | AnnotatedASTNode::TypeVariable(_, _, _) => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    return None;
                }
            }

            AnnotatedASTNode::Grouping(inner, _, _) => {
                if self.node_id() == id {
                    Some(self.clone())
                } else if let Some(inside) = inner {
                    inside.lookup(id)
                } else {
                    None
                }
            }
            AnnotatedASTNode::Tuple(items, _, _) => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    for i in items {
                        let inner_lookup = i.lookup(id);
                        if inner_lookup.is_some() {
                            return inner_lookup;
                        }
                    }
                    return None;
                }
            }
            AnnotatedASTNode::Declarations(items, _, _) => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    for i in items {
                        let inner_lookup = i.lookup(id);
                        if inner_lookup.is_some() {
                            return inner_lookup;
                        }
                    }
                    return None;
                }
            }
            AnnotatedASTNode::VariableDeclaration {
                pattern: _,
                value,
                type_declaration: _,
                location: _,
                node_id,
            } => {
                if self.node_id() == id {
                    Some(self.clone())
                } else {
                    value.lookup(id)
                }
            }
            AnnotatedASTNode::Binary {
                op: _,
                lhs,
                rhs,
                location: _,
                node_id: _,
            } => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    for i in [lhs, rhs] {
                        let inner_lookup = i.lookup(id);
                        if inner_lookup.is_some() {
                            return inner_lookup;
                        }
                    }
                    return None;
                }
            }
            AnnotatedASTNode::Unary {
                op: _,
                expr,
                location: _,
                node_id,
            } => {
                if id == self.node_id() {
                    Some(self.clone())
                } else {
                    expr.lookup(id)
                }
            }
            AnnotatedASTNode::If {
                condition,
                if_body,
                else_body,
                location: _,
                node_id,
            } => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    for i in [condition, if_body, else_body] {
                        let inner_lookup = i.lookup(id);
                        if inner_lookup.is_some() {
                            return inner_lookup;
                        }
                    }
                    return None;
                }
            }
            AnnotatedASTNode::Let {
                declarations,
                expr,
                location: _,
                node_id: _,
                environment: _,
            } => {
                if id == self.node_id() {
                    return Some(self.clone());
                } else {
                    for i in declarations {
                        let inner_lookup = i.lookup(id);
                        if inner_lookup.is_some() {
                            return inner_lookup;
                        }
                    }
                }
                return expr.lookup(id);
            }
        }
    }
    pub fn node_id(&self) -> usize {
        match self {
            AnnotatedASTNode::Error(_, node_id) => *node_id,
            AnnotatedASTNode::Int(_, _, node_id) => *node_id,
            AnnotatedASTNode::Float(_, _, node_id) => *node_id,
            AnnotatedASTNode::Bool(_, _, node_id) => *node_id,
            AnnotatedASTNode::Identifier(_, _, node_id) => *node_id,
            AnnotatedASTNode::TypeVariable(_, _, node_id) => *node_id,
            AnnotatedASTNode::Grouping(_, _, node_id) => *node_id,
            AnnotatedASTNode::Tuple(_, _, node_id) => *node_id,
            AnnotatedASTNode::Declarations(_, _, node_id) => *node_id,
            AnnotatedASTNode::VariableDeclaration {
                pattern: _,
                value: _,
                type_declaration: _,
                location: _,
                node_id,
            } => *node_id,
            AnnotatedASTNode::Binary {
                op: _,
                lhs: _,
                rhs: _,
                location: _,
                node_id,
            } => *node_id,
            AnnotatedASTNode::Unary {
                op: _,
                expr: _,
                location: _,
                node_id,
            } => *node_id,
            AnnotatedASTNode::If {
                condition: _,
                if_body: _,
                else_body: _,
                location: _,
                node_id,
            } => *node_id,
            AnnotatedASTNode::Let {
                declarations: _,
                expr: _,
                location: _,
                node_id,
                environment: _,
            } => *node_id,
        }
    }
}

impl SourcePosition for AnnotatedASTNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            AnnotatedASTNode::Error(location, _) => location.clone(),
            AnnotatedASTNode::Int(_, location, _) => location.clone(),
            AnnotatedASTNode::Float(_, location, _) => location.clone(),
            AnnotatedASTNode::Bool(_, location, _) => location.clone(),
            AnnotatedASTNode::Identifier(_, location, _) => location.clone(),
            AnnotatedASTNode::TypeVariable(_, location, _) => location.clone(),
            AnnotatedASTNode::Grouping(_, location, _) => location.clone(),
            AnnotatedASTNode::Tuple(_, location, _) => location.clone(),
            AnnotatedASTNode::Declarations(_, location, _) => location.clone(),
            AnnotatedASTNode::VariableDeclaration {
                pattern: _,
                value: _,
                type_declaration: _,
                location,
                node_id: _,
            } => location.clone(),
            AnnotatedASTNode::Binary {
                op: _,
                lhs: _,
                rhs: _,
                location,
                node_id: _,
            } => location.clone(),
            AnnotatedASTNode::Unary {
                op: _,
                expr: _,
                location,
                node_id: _,
            } => location.clone(),
            AnnotatedASTNode::If {
                condition: _,
                if_body: _,
                else_body: _,
                location,
                node_id: _,
            } => location.clone(),
            AnnotatedASTNode::Let {
                declarations: _,
                expr: _,
                location,
                node_id: _,
                environment: _,
            } => location.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypedASTNode {
    Error(SourceLocation),
    Int(i64, SourceLocation),
    Float(f64, SourceLocation),
    Bool(bool, SourceLocation),
    Identifier {
        name: String,
        node_type: Type,
        location: SourceLocation,
    },
    Grouping {
        expr: Option<Box<TypedASTNode>>,
        node_type: Type,
        location: SourceLocation,
    },
    Tuple {
        exprs: Vec<TypedASTNode>,
        node_type: Type,
        location: SourceLocation,
    },
    Declarations {
        declarations: Vec<TypedASTNode>,
        node_type: Type,
        location: SourceLocation,
    },
    VariableDeclaration {
        variable: String,
        value: Box<TypedASTNode>,
        node_type: Type,
        location: SourceLocation,
    },

    Binary {
        node_type: Type,
        op: Operator,
        lhs: Box<TypedASTNode>,
        rhs: Box<TypedASTNode>,
        location: SourceLocation,
    },
    Unary {
        node_type: Type,
        op: Operator,
        expr: Box<TypedASTNode>,
        location: SourceLocation,
    },

    If {
        node_type: Type,
        condition: Box<TypedASTNode>,
        if_body: Box<TypedASTNode>,
        else_body: Box<TypedASTNode>,
        location: SourceLocation,
    },
    Let {
        node_type: Type,
        declarations: Vec<TypedASTNode>,
        expr: Box<TypedASTNode>,
        location: SourceLocation,
        environment: Option<Box<SymbolTable<Type>>>,
    },
}

impl TypedASTNode {
    pub fn get_type(&self) -> Type {
        match self {
            TypedASTNode::Error(_) => Type::Unknown,
            TypedASTNode::Int(_, _) => Type::Int,
            TypedASTNode::Float(_, _) => Type::Float,
            TypedASTNode::Bool(_, _) => Type::Bool,
            TypedASTNode::Identifier {
                location: _,
                name: _,
                node_type: t,
            } => t.clone(),
            TypedASTNode::VariableDeclaration {
                location: _,
                variable: _,
                value: _,
                node_type: t,
            } => t.clone(),
            TypedASTNode::Declarations {
                location: _,
                declarations: _,
                node_type: t,
            } => t.clone(),
            TypedASTNode::Tuple {
                location: _,
                exprs: _,
                node_type: t,
            } => t.clone(),
            TypedASTNode::Grouping {
                location: _,
                expr: _,
                node_type: t,
            } => t.clone(),
            TypedASTNode::Unary {
                location: _,
                node_type: t,
                op: _,
                expr: _,
            } => t.clone(),
            TypedASTNode::Binary {
                location: _,
                node_type: t,
                op: _,
                lhs: _,
                rhs: _,
            } => t.clone(),
            TypedASTNode::If {
                location: _,
                node_type: t,
                condition: _,
                if_body: _,
                else_body: _,
            } => t.clone(),
            TypedASTNode::Let {
                node_type: t,
                declarations: _,
                expr: _,
                location: _,
                environment: _,
            } => t.clone(),
        }
    }
}

impl SourcePosition for TypedASTNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            TypedASTNode::Error(location) => location.clone(),
            TypedASTNode::Int(_, location) => location.clone(),
            TypedASTNode::Float(_, location) => location.clone(),
            TypedASTNode::Bool(_, location) => location.clone(),
            TypedASTNode::Identifier {
                name: _,
                node_type: _,
                location,
            } => location.clone(),
            TypedASTNode::Grouping {
                expr: _,
                node_type: _,
                location,
            } => location.clone(),
            TypedASTNode::Tuple {
                node_type: _,
                exprs: _,
                location,
            } => location.clone(),
            TypedASTNode::Declarations {
                node_type: _,
                declarations: _,
                location,
            } => location.clone(),
            TypedASTNode::VariableDeclaration {
                node_type: _,
                variable: _,
                value: _,
                location,
            } => location.clone(),
            TypedASTNode::Binary {
                node_type: _,
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => location.clone(),
            TypedASTNode::Unary {
                node_type: _,
                op: _,
                expr: _,
                location,
            } => location.clone(),
            TypedASTNode::If {
                node_type: _,
                condition: _,
                if_body: _,
                else_body: _,
                location,
            } => location.clone(),
            TypedASTNode::Let {
                node_type: _,
                declarations: _,
                expr: _,
                location,
                environment: _,
            } => location.clone(),
        }
    }
}
