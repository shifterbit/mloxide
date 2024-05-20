use crate::source_location::{SourceLocation, SourcePosition};

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

#[derive(Debug, Clone)]
pub enum AstNode {
    Error(SourceLocation),
    Int(i64, SourceLocation),
    Float(f64, SourceLocation),
    Bool(bool, SourceLocation),
    Identifier(String, SourceLocation),
    Grouping(Box<AstNode>, SourceLocation),
    Declarations(Vec<AstNode>, SourceLocation),
    VariableDeclaration {
        variable: String,
        value: Box<AstNode>,
        location: SourceLocation,
    },
    Binary {
        op: Operator,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        location: SourceLocation,
    },
    Unary {
        op: Operator,
        expr: Box<AstNode>,
        location: SourceLocation,
    },
    If {
        condition: Box<AstNode>,
        if_body: Box<AstNode>,
        else_body: Box<AstNode>,
        location: SourceLocation,
    },
}

impl SourcePosition for AstNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            AstNode::Error(location) => *location,
            AstNode::Int(_, location) => *location,
            AstNode::Float(_, location) => *location,
            AstNode::Bool(_, location) => *location,
            AstNode::Identifier(_, location) => *location,
            AstNode::Grouping(_, location) => *location,
            AstNode::Declarations(_, location) => *location,
            AstNode::VariableDeclaration {
                variable: _,
                value: _,
                location,
            } => *location,
            AstNode::Binary {
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => *location,
            AstNode::Unary {
                op: _,
                expr: _,
                location,
            } => *location,
            AstNode::If {
                condition: _,
                if_body: _,
                else_body: _,
                location,
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
    Unknown,
}

#[derive(Clone, Debug)]
pub enum TypedAstNode {
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
        expr: Box<TypedAstNode>,
        node_type: Type,
        location: SourceLocation,
    },
    Declarations {
        declarations: Vec<TypedAstNode>,
        node_type: Type,
        location: SourceLocation,
    },
    VariableDeclaration {
        variable: String,
        value: Box<TypedAstNode>,
        node_type: Type,
        location: SourceLocation,
    },

    Binary {
        node_type: Type,
        op: Operator,
        lhs: Box<TypedAstNode>,
        rhs: Box<TypedAstNode>,
        location: SourceLocation,
    },
    Unary {
        node_type: Type,
        op: Operator,
        expr: Box<TypedAstNode>,
        location: SourceLocation,
    },

    If {
        node_type: Type,
        condition: Box<TypedAstNode>,
        if_body: Box<TypedAstNode>,
        else_body: Box<TypedAstNode>,
        location: SourceLocation,
    },
}

impl TypedAstNode {
    pub fn get_type(&self) -> Type {
        match self {
            TypedAstNode::Error(_) => Type::Unknown,
            TypedAstNode::Int(_, _) => Type::Int,
            TypedAstNode::Float(_, _) => Type::Float,
            TypedAstNode::Bool(_, _) => Type::Bool,
            TypedAstNode::Identifier {
                location: _,
                name: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::VariableDeclaration {
                location: _,
                variable: _,
                value: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Declarations {
                location: _,
                declarations: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Grouping {
                location: _,
                expr: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Unary {
                location: _,
                node_type: t,
                op: _,
                expr: _,
            } => t.clone(),
            TypedAstNode::Binary {
                location: _,
                node_type: t,
                op: _,
                lhs: _,
                rhs: _,
            } => t.clone(),
            TypedAstNode::If {
                location: _,
                node_type: t,
                condition: _,
                if_body: _,
                else_body: _,
            } => t.clone(),
        }
    }
}

impl SourcePosition for TypedAstNode {
    fn source_location(&self) -> SourceLocation {
        match self {
            TypedAstNode::Error(location) => *location,
            TypedAstNode::Int(_, location) => *location,
            TypedAstNode::Float(_, location) => *location,
            TypedAstNode::Bool(_, location) => *location,
            TypedAstNode::Identifier {
                name: _,
                node_type: _,
                location,
            } => *location,
            TypedAstNode::Grouping {
                expr: _,
                node_type: _,
                location,
            } => *location,
            TypedAstNode::Declarations {
                node_type: _,
                declarations: _,
                location,
            } => *location,
            TypedAstNode::VariableDeclaration {
                node_type: _,
                variable: _,
                value: _,
                location,
            } => *location,
            TypedAstNode::Binary {
                node_type: _,
                op: _,
                lhs: _,
                rhs: _,
                location,
            } => *location,
            TypedAstNode::Unary {
                node_type: _,
                op: _,
                expr: _,
                location,
            } => *location,
            TypedAstNode::If {
                node_type: _,
                condition: _,
                if_body: _,
                else_body: _,
                location,
            } => *location,
        }
    }
}
