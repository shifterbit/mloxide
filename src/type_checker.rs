use crate::ast::{AstNode, Operator};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Int,
    Real,
    Bool,
    Unknown,
}

#[derive(Clone, Debug)]
pub enum TypedAstNode {
    Int(i64),
    Real(f64),
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
            TypedAstNode::Real(_) => Type::Real,
            TypedAstNode::Unary {
                node_type: _,
                op: _,
                expr,
            } => {
                let expr_type = expr.get_type();
                return expr_type;
            }
            TypedAstNode::Binary {
                node_type: _,
                op: _,
                lhs,
                rhs,
            } => {
                let lhs_type = lhs.get_type();
                let rhs_type = rhs.get_type();
                if lhs_type == rhs_type {
                    return lhs_type;
                } else {
                    return Type::Unknown;
                }
            }
        }
    }
}

pub fn typecheck(node: Box<AstNode>) -> TypedAstNode {
    match *node {
        AstNode::Int(n) => TypedAstNode::Int(n),
        AstNode::Real(n) => TypedAstNode::Real(n),
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
        Operator::Plus | Operator::Minus | Operator::Negation | Operator::Multiply => {
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
        Operator::IntDivision => return Type::Int,
        Operator::RealDivision => return Type::Real,
        _ => return Type::Unknown,
    }
}

fn operator_return_types(operator: Operator) -> Vec<Type> {
    match operator {
        Operator::Plus | Operator::Minus | Operator::Negation | Operator::Multiply => {
            return vec![Type::Int, Type::Real];
        }
        Operator::Equal | Operator::NotEqual => {
            return vec![Type::Bool];
        }
        Operator::IntDivision => {
            return vec![Type::Int];
        }
        Operator::RealDivision => {
            return vec![Type::Real];
        }
    }
}
fn allowed_binary_op_type(operator: Operator, left_type: Type) -> (Type, Type) {
    match operator {
        Operator::Plus | Operator::Minus | Operator::Multiply => match left_type {
            Type::Int => return (Type::Int, Type::Int),
            Type::Real => return (Type::Real, Type::Real),
            _ => panic!("Invalid Type"),
        },
        Operator::Equal | Operator::NotEqual => match left_type {
            t => return (t.clone(), t),
        },
        Operator::IntDivision => return (Type::Int, Type::Int),
        Operator::RealDivision => return (Type::Real, Type::Real),
        _ => panic!("Expected Binary Operator"),
    }
}

fn allowed_infix_op_type(operator: Operator, expr_type: Type) -> Type {
    match operator {
        Operator::Negation => {
            let allowed_types = vec![Type::Int, Type::Real];
            if allowed_types.contains(&expr_type) {
                return expr_type;
            } else {
                return Type::Unknown;
            }
        }
        _ => Type::Unknown,
    }
}
