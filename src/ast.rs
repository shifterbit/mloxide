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
    Error,
    Int(i64),
    Float(f64),
    Bool(bool),
    Identifier(String),
    Grouping(Box<AstNode>),
    Declarations(Vec<AstNode>),
    VariableDeclaration {
        variable: String,
        value: Box<AstNode>,
    },
    Binary {
        op: Operator,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Unary {
        op: Operator,
        expr: Box<AstNode>,
    },
    If {
        condition: Box<AstNode>,
        if_body: Box<AstNode>,
        else_body: Box<AstNode>,
    },
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
    Error,
    Int(i64),
    Float(f64),
    Bool(bool),
    Identifier {
        name: String,
        node_type: Type,
    },
    Grouping {
        expr: Box<TypedAstNode>,
        node_type: Type,
    },
    Declarations {
        declarations: Vec<TypedAstNode>,
        node_type: Type,
    },
    VariableDeclaration {
        variable: String,
        value: Box<TypedAstNode>,
        node_type: Type,
    },

    Binary {
        node_type: Type,
        op: Operator,
        lhs: Box<TypedAstNode>,
        rhs: Box<TypedAstNode>,
    },
    Unary {
        node_type: Type,
        op: Operator,
        expr: Box<TypedAstNode>,
    },
    If {
        node_type: Type,
        condition: Box<TypedAstNode>,
        if_body: Box<TypedAstNode>,
        else_body: Box<TypedAstNode>,
    },
}

impl TypedAstNode {
    pub fn get_type(&self) -> Type {
        match self {
            TypedAstNode::Error => Type::Unknown,
            TypedAstNode::Int(_) => Type::Int,
            TypedAstNode::Float(_) => Type::Float,
            TypedAstNode::Bool(_) => Type::Bool,
            TypedAstNode::Identifier {
                name: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::VariableDeclaration {
                variable: _,
                value: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Declarations {
                declarations: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Grouping {
                expr: _,
                node_type: t,
            } => t.clone(),
            TypedAstNode::Unary {
                node_type: t,
                op: _,
                expr: _,
            } => t.clone(),
            TypedAstNode::Binary {
                node_type: t,
                op: _,
                lhs: _,
                rhs: _,
            } => t.clone(),
            TypedAstNode::If {
                node_type: t,
                condition: _,
                if_body: _,
                else_body: _,
            } => t.clone(),
        }
    }
}
