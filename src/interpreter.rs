use std::fmt::{self, Display};

use crate::{
    ast::{Operator, TypedASTNode},
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Unit => write!(f, ""),
            Value::Tuple(values) => {
                let mut msg = "(".to_string();
                for value in values {
                    msg.push_str(&(value.to_string() + ", "));
                }
                msg = msg.strip_suffix(", ").unwrap().to_string();
                msg.push(')');
                write!(f, "{}", msg)
            }
        }
    }
}
pub fn  eval(ast: TypedASTNode) -> Value {
    let mut value_table: SymbolTable<Value> = SymbolTable::new();
    eval_expression(ast, &mut value_table)
    
}
pub fn eval_expression(ast: TypedASTNode, symbol_table: &mut SymbolTable<Value>) -> Value {
    match ast {
        TypedASTNode::Int(n, _) => Value::Int(n),
        TypedASTNode::Float(n, _) => Value::Float(n),
        TypedASTNode::Bool(b, _) => Value::Bool(b),
        TypedASTNode::Identifier {
            name,
            node_type: _,
            location: _,
        } => match symbol_table.lookup(&name) {
            Some(v) => v,
            None => panic!("Variable does not exist"),
        },
        TypedASTNode::Grouping {
            expr,
            node_type: _,
            location: _,
        } => match expr {
            Some(exp) => eval_expression(*exp, symbol_table),
            None => Value::Unit,
        },
        TypedASTNode::Tuple {
            exprs,
            node_type: _,
            location: _,
        } => {
            let values: Vec<Value> = exprs
                .iter()
                .map(|expr| eval_expression(expr.clone(), symbol_table))
                .collect();
            Value::Tuple(values)
        }
        TypedASTNode::Binary {
            node_type: _,
            op,
            lhs,
            rhs,
            location: _,
        } => eval_binary(op, *lhs, *rhs, symbol_table),
        TypedASTNode::Unary {
            node_type: _,
            op,
            expr,
            location: _,
        } => eval_unary(op, *expr, symbol_table),
        TypedASTNode::If {
            node_type: _,
            condition,
            if_body,
            else_body,
            location: _,
        } => eval_if_expression(*condition, *if_body, *else_body, symbol_table),
        TypedASTNode::Let {
            node_type: _,
            declarations,
            expr,
            location: _,
            environment: _,
        } => {
            let mut scoped_symbol_table = symbol_table.enter_scope();
            for declaration in declarations {
                eval_expression(declaration, &mut scoped_symbol_table);
            }
            eval_expression(*expr, &mut scoped_symbol_table)
        }
        TypedASTNode::VariableDeclaration {
            variable,
            value,
            node_type: _,
            location: _,
        } => {
            let val = eval_expression(*value, symbol_table);
            symbol_table.insert(&variable, val);
            Value::Unit
        }
        TypedASTNode::Declarations {
            declarations,
            node_type: _,
            location: _,
        } => {
            let mut values: Vec<Value> = Vec::new();
            for declaration in declarations {
                let value = eval_expression(declaration, symbol_table);
                values.push(value)
            }
            values.last().unwrap().clone()
        }
        TypedASTNode::Error(_) => {
            panic!("This should not happen");
        }
    }
}

fn eval_if_expression(
    condition: TypedASTNode,
    if_body: TypedASTNode,
    else_body: TypedASTNode,
    symbol_table: &mut SymbolTable<Value>,
) -> Value {
    let condition = eval_expression(condition, symbol_table);
    match condition {
        Value::Bool(true) => eval_expression(if_body, symbol_table),
        Value::Bool(false) => eval_expression(else_body, symbol_table),
        _ => panic!("Expected Boolean Value for condition"),
    }
}

fn eval_binary(
    op: Operator,
    left: TypedASTNode,
    right: TypedASTNode,
    symbol_table: &mut SymbolTable<Value>,
) -> Value {
    let lhs = eval_expression(left, symbol_table);
    let rhs = eval_expression(right, symbol_table);

    match op {
        Operator::Equal => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l == r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l == r),
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l == r),
            (Value::Tuple(t1), Value::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    Value::Bool(false)
                } else {
                    for i in 0..t1.len() {
                        if t1[i] != t2[i] {
                            return Value::Bool(false);
                        }
                    }
                    Value::Bool(true)
                }
            }
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
            _ => panic!("Type Error During Comparison"),
        },
        Operator::LessThan => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l < r),
            _ => panic!("Type Error During Comparison"),
        },
        Operator::LessEqual => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l <= r),
            _ => panic!("Type Error During Comparison"),
        },
        Operator::GreaterEqual => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l >= r),
            _ => panic!("Type Error During Comparison"),
        },
        Operator::GreaterThan => match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l > r),
            _ => panic!("Type Error During Comparison"),
        },

        _ => panic!("Invalid Binary Operator"),
    }
}

fn eval_unary(op: Operator, right: TypedASTNode, symbol_table: &mut SymbolTable<Value>) -> Value {
    let expr = eval_expression(right, symbol_table);

    match op {
        Operator::Negate => match expr {
            Value::Int(n) => Value::Int(-n),
            Value::Float(n) => Value::Float(-n),
            Value::Bool(_b) => panic!("Cannot negate type bool"),
            Value::Unit => panic!("Cannot negate unit type"),
            Value::Tuple(_) => panic!("Cannot negate tuple"),
        },
        _ => panic!("Invalid Unary Operator"),
    }
}
