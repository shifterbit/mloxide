use std::{fmt::{self, Display}};

use crate::{ast::{Operator, TypedAstNode}, name_resolution::SymbolTable};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Unit => write!(f, ""),
        }
    }
}

pub fn eval_expression(ast: TypedAstNode, symbol_table: &mut SymbolTable<Value>) -> Value {
    match ast {
        TypedAstNode::Int(n, _) => Value::Int(n),
        TypedAstNode::Float(n, _) => Value::Float(n),
        TypedAstNode::Bool(b, _) => Value::Bool(b),
        TypedAstNode::Identifier { name, node_type: _, location: _ } => {
            match symbol_table.lookup(&name) {
                Some(v) =>  v,
                None => panic!("Variable does not exist")
            }
        },
        TypedAstNode::Grouping { expr, node_type: _, location: _ } => eval_expression(*expr, symbol_table),
        TypedAstNode::Binary {
            node_type: _,
            op,
            lhs,
            rhs,
            location: _
        } => eval_binary(op, *lhs, *rhs, symbol_table),
        TypedAstNode::Unary {
            node_type: _,
            op,
            expr,
            location: _,
        } => eval_unary(op, *expr, symbol_table),
        TypedAstNode::If {
            node_type: _,
            condition,
            if_body,
            else_body,
            location: _
        } => eval_if_expression(*condition, *if_body, *else_body, symbol_table),
        TypedAstNode::VariableDeclaration { variable, value, node_type: _, location: _ } => {
            let val = eval_expression(*value, symbol_table);
            symbol_table.insert(&variable, val);
            Value::Unit
        },
        TypedAstNode::Declarations{ declarations, node_type: _, location: _ } => {
            let mut values: Vec<Value> = Vec::new();
            for declaration in declarations {
                let value = eval_expression(declaration, symbol_table);
                values.push(value)
            }
            values.last().unwrap().clone()
        }
        TypedAstNode::Error(_) => {
            panic!("This should not happen");
        }
    }
}

fn eval_if_expression(
    condition: TypedAstNode,
    if_body: TypedAstNode,
    else_body: TypedAstNode,
    symbol_table: &mut SymbolTable<Value>
) -> Value {
    let condition = eval_expression(condition, symbol_table);
    match condition {
        Value::Bool(true) => eval_expression(if_body, symbol_table),
        Value::Bool(false) => eval_expression(else_body, symbol_table),
        _ => panic!("Expected Boolean Value for condition"),
    }
}

fn eval_binary(op: Operator, left: TypedAstNode, right: TypedAstNode, symbol_table: &mut SymbolTable<Value>) -> Value {
    let lhs = eval_expression(left, symbol_table);
    let rhs = eval_expression(right, symbol_table);

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

fn eval_unary(op: Operator, right: TypedAstNode, symbol_table: &mut SymbolTable<Value>) -> Value {
    let expr = eval_expression(right, symbol_table);

    match op {
        Operator::Negate => match expr {
            Value::Int(n) => Value::Int(-n),
            Value::Float(n) => Value::Float(-n),
            Value::Bool(_b) => panic!("Cannot negate type bool"),
            Value::Unit => panic!("Cannot negate unit type")
        },
        _ => panic!("Invalid Unary Operator"),
    }
}
