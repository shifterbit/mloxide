use crate::ast::{AstNode, Operator};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unknown,
}

#[derive(Clone, Debug)]
pub enum TypedAstNode {
    Int(i64),
    Float(f64),
    Bool(bool),
    Identifier(String),
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
}

impl TypedAstNode {
    pub fn get_type(self: &Self) -> Type {
        match self {
            TypedAstNode::Int(_) => Type::Int,
            TypedAstNode::Float(_) => Type::Float,
            TypedAstNode::Bool(_) => Type::Bool,
            TypedAstNode::Identifier(_) => Type::Unknown,
            TypedAstNode::Unary {
                node_type: t,
                op: _,
                expr: _,
            } => return t.clone(),
            TypedAstNode::Binary {
                node_type: t,
                op: _,
                lhs: _,
                rhs: _,
            } => t.clone(),
        }
    }
}

pub fn typecheck(node: Box<AstNode>) -> TypedAstNode {
    match *node {
        AstNode::Int(n) => TypedAstNode::Int(n),
        AstNode::Float(n) => TypedAstNode::Float(n),
        AstNode::Bool(b) => TypedAstNode::Bool(b),
        AstNode::Identifier(i) => TypedAstNode::Identifier(i),
        AstNode::Binary { op, lhs, rhs } => {
            let typed_lhs = typecheck(lhs);
            let typed_rhs = typecheck(rhs);
            let l_type = typed_lhs.get_type();
            let r_type = typed_rhs.get_type();
            let (l_expected, r_expected) = allowed_binary_op_type(op, l_type.clone());

            let full_type = if (&l_expected, &r_expected) == (&l_type, &r_type) {
                binary_return_type(op, l_type, r_type)
            } else {
                Type::Unknown
            };

            return TypedAstNode::Binary {
                node_type: full_type,
                op,
                lhs: Box::new(typed_lhs),
                rhs: Box::new(typed_rhs),
            };
        }
        AstNode::Unary { op, expr } => {
            let typed_expr = typecheck(expr);
            let e_type = typed_expr.get_type();
            let expected_type = allowed_infix_op_type(op, e_type.clone());
            return TypedAstNode::Unary {
                node_type: expected_type,
                op,
                expr: Box::new(typed_expr),
            };
        }
    }
}

fn binary_return_type(operator: Operator, left_type: Type, right_type: Type) -> Type {
    match operator {
        Operator::Add | Operator::Subtract | Operator::Negate | Operator::Multiply | Operator::Divide => {
            let allowed_types = operator_return_types(operator);
            let left_valid = allowed_types.contains(&left_type);
            let right_valid = allowed_types.contains(&right_type);
            let both_valid = left_valid && right_valid;
            if allowed_types.contains(&left_type) && both_valid {
                return left_type;
            } else {
                return Type::Unknown;
            }
        }
        Operator::Equal | Operator::NotEqual => return Type::Bool,
    }
}

fn operator_return_types(operator: Operator) -> Vec<Type> {
    match operator {
        Operator::Add | Operator::Subtract | Operator::Negate | Operator::Multiply | Operator::Divide => {
            return vec![Type::Int, Type::Float];
        }
        Operator::Equal | Operator::NotEqual => {
            return vec![Type::Bool];
        }

    }
}
fn allowed_binary_op_type(operator: Operator, left_type: Type) -> (Type, Type) {
    match operator {
        Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => match left_type {
            Type::Int => return (Type::Int, Type::Int),
            Type::Float => return (Type::Float, Type::Float),
            _ => panic!("Invalid Type"),
        },
        Operator::Equal | Operator::NotEqual => match left_type {
            t => return (t.clone(), t),
        },
        _ => panic!("Expected Binary Operator"),
    }
}

fn allowed_infix_op_type(operator: Operator, expr_type: Type) -> Type {
    match operator {
        Operator::Negate => {
            let allowed_types = vec![Type::Int, Type::Float];
            if allowed_types.contains(&expr_type) {
                return expr_type;
            } else {
                return Type::Unknown;
            }
        }
        _ => Type::Unknown,
    }
}
