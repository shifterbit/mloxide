use crate::{ast::Operator, type_checker::TypedAstNode};

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
}

pub fn eval_expression(ast: TypedAstNode) -> Value {
    match ast {
        TypedAstNode::Int(n) => Value::Int(n),
        TypedAstNode::Float(n) => Value::Float(n),
        TypedAstNode::Binary {
            node_type: _,
            op,
            lhs,
            rhs,
        } => {
            eval_binary(op, *lhs, *rhs)
        },
        TypedAstNode::Unary {
            node_type: _,
            op,
            expr,
        } => eval_unary(op, *expr),
    }
}

fn eval_binary(op: Operator, left: TypedAstNode, right: TypedAstNode) -> Value {
    let lhs = eval_expression(left);
    let rhs = eval_expression(right);

    match op {
        Operator::Equal => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l == r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l == r),
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l == r),
            _ => panic!("Type Error During Equality Check"),
        },
        Operator::NotEqual => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l != r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l != r),
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l != r),
            _ => panic!("Type Error During Equality Check"),
        },
        Operator::Add => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
            _ => panic!("Type Error During Addition"),
        },
        Operator::Subtract => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
            _ => panic!("Type Error During Subtraction"),
        },

        Operator::Multiply => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
            _ => panic!("Type Error During  Multiplication"),
        },
        Operator::Divide => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
            _ => panic!("Type Error During Division"),
        },
        _ => panic!("Invalid Binary Operator"),
    }
}

fn eval_unary(op: Operator, right: TypedAstNode) -> Value {
    let expr = eval_expression(right);

    match op {
        Operator::Negate => match expr {
            Value::Int(n) => Value::Int(-n),
            Value::Float(n) => Value::Float(-n),
            Value::Bool(_b) => panic!("Cannot negate type bool"),
        },
        _ => panic!("Invalid Unary Operator"),
    }
}
