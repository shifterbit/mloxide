use crate::ast::{AstNode, Operator};

#[derive(PartialEq, Eq, Clone, Debug)]
enum Type {
    Int,
    Real,
    Unknown,
}

#[derive(Clone, Debug)]
pub enum TypedAstNode {
    Int(i64),
    Real(f64),
    Binary {
        nodeType: Type,
        op: Operator,
        lhs: Box<TypedAstNode>,
        rhs: Box<TypedAstNode>,
    },
    Unary {
        nodeType: Type,
        op: Operator,
        expr: Box<TypedAstNode>,
    },
}

impl TypedAstNode {
    pub fn get_type(self: &Self) -> Type {
        match self {
            TypedAstNode::Int(_) => Type::Int,
            TypedAstNode::Real(_) => Type::Real,
            TypedAstNode::Unary { nodeType, op, expr } => {
                let expr_type = expr.get_type();
                return expr_type;
            }
            TypedAstNode::Binary {
                nodeType,
                op,
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

            let full_type = if l_type == r_type && l_type != Type::Unknown {
                l_type
            } else {
                println!("Expected types ({l_type:#?} {op:?} {l_type:#?}) got ({l_type:#?} {op:?} {r_type:#?})");
                Type::Unknown
            };

            return TypedAstNode::Binary {
                nodeType: full_type,
                op,
                lhs: Box::new(typed_lhs),
                rhs: Box::new(typed_rhs),
            };
        }
        AstNode::Unary { op, expr } => {
            let typed_expr = typecheck(expr);
            let e_type = typed_expr.get_type();
            return TypedAstNode::Unary { nodeType: e_type, op, expr: Box::new(typed_expr) };
        }
    }
}
