use std::fmt::{self, Display};

use crate::{
    source_location::{SourceLocation, SourcePosition},
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Negate,
    Divide,
    Multiply,
    Equal,
    NotEqual,
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Error(SourceLocation),
    Int(i64, SourceLocation),
    Float(f64, SourceLocation),
    Bool(bool, SourceLocation),
    Identifier(String, SourceLocation),
    Grouping(Option<Box<ASTNode>>, SourceLocation),
    Declarations(Vec<ASTNode>, SourceLocation),
    VariableDeclaration {
        variable: String,
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

impl SourcePosition for ASTNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            ASTNode::Error(location) => *location,
            ASTNode::Int(_, location) => *location,
            ASTNode::Float(_, location) => *location,
            ASTNode::Bool(_, location) => *location,
            ASTNode::Identifier(_, location) => *location,
            ASTNode::Grouping(_, location) => *location,
            ASTNode::Declarations(_, location) => *location,
            ASTNode::VariableDeclaration {
                variable: _,
                value: _,
                location,
            } => *location,
            ASTNode::Binary {
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => *location,
            ASTNode::Unary {
                op: _,
                expr: _,
                location,
            } => *location,
            ASTNode::If {
                condition: _,
                if_body: _,
                else_body: _,
                location,
            } => *location,
            ASTNode::Let {
                declarations: _,
                expr: _,
                location,
                environment: _,
            } => *location,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Declarations(Vec<Type>),
    Unit,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "()"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Declarations(declarations) => {
                let mut msg = "[".to_string();
                for decl in declarations {
                    msg.push_str(&(" ".to_string() + &decl.to_string()));
                }
                msg.push_str(" ]");
                write!(f, "{}", msg)
            }
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
            TypedASTNode::Error(location) => *location,
            TypedASTNode::Int(_, location) => *location,
            TypedASTNode::Float(_, location) => *location,
            TypedASTNode::Bool(_, location) => *location,
            TypedASTNode::Identifier {
                name: _,
                node_type: _,
                location,
            } => *location,
            TypedASTNode::Grouping {
                expr: _,
                node_type: _,
                location,
            } => *location,
            TypedASTNode::Declarations {
                node_type: _,
                declarations: _,
                location,
            } => *location,
            TypedASTNode::VariableDeclaration {
                node_type: _,
                variable: _,
                value: _,
                location,
            } => *location,
            TypedASTNode::Binary {
                node_type: _,
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => *location,
            TypedASTNode::Unary {
                node_type: _,
                op: _,
                expr: _,
                location,
            } => *location,
            TypedASTNode::If {
                node_type: _,
                condition: _,
                if_body: _,
                else_body: _,
                location,
            } => *location,
            TypedASTNode::Let {
                node_type: _,
                declarations: _,
                expr: _,
                location,
                environment: _,
            } => *location,
        }
    }
}
