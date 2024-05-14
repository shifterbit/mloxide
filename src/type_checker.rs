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
    Grouping {
        expr: Box<TypedAstNode>,
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
            TypedAstNode::Int(_) => Type::Int,
            TypedAstNode::Float(_) => Type::Float,
            TypedAstNode::Bool(_) => Type::Bool,
            TypedAstNode::Identifier(_) => Type::Unknown,
            TypedAstNode::Grouping { expr: _, node_type: t } => t.clone(),
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
            TypedAstNode::If { node_type: t, condition: _, if_body: _, else_body: _ } => t.clone()
        }
    }
}

pub fn typecheck(node: AstNode) -> TypedAstNode {
    match node {
        AstNode::Int(n) => TypedAstNode::Int(n),
        AstNode::Float(n) => TypedAstNode::Float(n),
        AstNode::Bool(b) => TypedAstNode::Bool(b),
        AstNode::Identifier(i) => TypedAstNode::Identifier(i),
        AstNode::Grouping(expr) => {
            let typecheck = typecheck(*expr);
            let typed_expr = typecheck;
            let expr_type = typed_expr.get_type();
            TypedAstNode::Grouping { expr: Box::new(typed_expr), node_type: expr_type }
        },
        AstNode::Binary { op, lhs, rhs } => {
            let typed_lhs = typecheck(*lhs);
            let typed_rhs = typecheck(*rhs);
            let l_type = typed_lhs.get_type();
            let r_type = typed_rhs.get_type();
            let (l_expected, r_expected) = allowed_binary_op_type(op, l_type.clone());

            let full_type = if (&l_expected, &r_expected) == (&l_type, &r_type) {
                binary_return_type(op, l_type, r_type)
            } else {
                Type::Unknown
            };

            TypedAstNode::Binary {
                node_type: full_type,
                op,
                lhs: Box::new(typed_lhs),
                rhs: Box::new(typed_rhs),
            }
        }
        AstNode::Unary { op, expr } => {
            let typed_expr = typecheck(*expr);
            let e_type = typed_expr.get_type();
            let expected_type = allowed_infix_op_type(op, e_type.clone());
            TypedAstNode::Unary {
                node_type: expected_type,
                op,
                expr: Box::new(typed_expr),
            }
        }
        AstNode::If {
            condition,
            if_body,
            else_body,
        } => {
            let condition_typed = typecheck(*condition);
            let if_body_typed = typecheck(*if_body);
            let else_body_typed = typecheck(*else_body);

            let full_type = if if_body_typed.get_type() == else_body_typed.get_type() {
                if_body_typed.get_type()
            } else {
                Type::Unknown
            };


            TypedAstNode::If {
                node_type: full_type,
                condition: Box::new(condition_typed),
                if_body: Box::new(if_body_typed),
                else_body: Box::new(else_body_typed),
            }
        }
    }
}

fn binary_return_type(operator: Operator, left_type: Type, right_type: Type) -> Type {
    match operator {
        Operator::Add
        | Operator::Subtract
        | Operator::Negate
        | Operator::Multiply
        | Operator::Divide => {
            let allowed_types = operator_return_types(operator);
            let left_valid = allowed_types.contains(&left_type);
            let right_valid = allowed_types.contains(&right_type);
            let both_valid = left_valid && right_valid;
            if allowed_types.contains(&left_type) && both_valid {
                left_type
            } else {
                Type::Unknown
            }
        }
        Operator::Equal | Operator::NotEqual => Type::Bool,
    }
}

fn operator_return_types(operator: Operator) -> Vec<Type> {
    match operator {
        Operator::Add
        | Operator::Subtract
        | Operator::Negate
        | Operator::Multiply
        | Operator::Divide => {
            vec![Type::Int, Type::Float]
        }
        Operator::Equal | Operator::NotEqual => {
            vec![Type::Bool]
        }
    }
}
fn allowed_binary_op_type(operator: Operator, left_type: Type) -> (Type, Type) {
    match operator {
        Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
            match left_type {
                Type::Int => (Type::Int, Type::Int),
                Type::Float => (Type::Float, Type::Float),
                _ => panic!("Invalid Type"),
            }
        }
        Operator::Equal | Operator::NotEqual => (left_type.clone(), left_type),
        _ => panic!("Expected Binary Operator"),
    }
}

fn allowed_infix_op_type(operator: Operator, expr_type: Type) -> Type {
    match operator {
        Operator::Negate => {
            let allowed_types = vec![Type::Int, Type::Float];
            if allowed_types.contains(&expr_type) {
                expr_type
            } else {
                Type::Unknown
            }
        }
        _ => Type::Unknown,
    }
}
