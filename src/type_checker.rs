use std::fmt::{self, Display};

use crate::{
    ast::{AstNode, Operator, Type, TypedAstNode},
    error_reporting::CompilerError,
    name_resolution::SymbolTable,
    source_location::{SourceLocation, SourcePosition},
};

pub type TypeErrorList = Vec<TypeError>;
#[derive(Debug)]
pub struct TypeError {
    message: String,
    location: SourceLocation,
}

impl CompilerError for TypeError {
    fn new(message: &str, location: SourceLocation) -> TypeError {
        TypeError {
            message: message.to_string(),
            location,
        }
    }

    fn location(&self) -> SourceLocation {
        self.location
    }

    fn message(&self) -> &str {
        &self.message
    }

    fn error_type(&self) -> &str {
        "TypeError"
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn check_types(
    ast: AstNode,
    symbol_table: &SymbolTable<AstNode>,
) -> Result<TypedAstNode, TypeErrorList> {
    let mut type_table: SymbolTable<Type> = SymbolTable::new();
    let mut errors: TypeErrorList = Vec::new();
    let res = typecheck(ast, symbol_table, &mut type_table, &mut errors);
    if errors.is_empty() {
        Ok(res)
    } else {
        Err(errors)
    }
}

pub fn typecheck(
    node: AstNode,
    symbol_table: &SymbolTable<AstNode>,
    type_table: &mut SymbolTable<Type>,
    errors: &mut TypeErrorList,
) -> TypedAstNode {
    match node {
        AstNode::Error(loc) => TypedAstNode::Error(loc),
        AstNode::Int(n, loc) => TypedAstNode::Int(n, loc),
        AstNode::Float(n, loc) => TypedAstNode::Float(n, loc),
        AstNode::Bool(b, loc) => TypedAstNode::Bool(b, loc),
        AstNode::Identifier(i, loc) => match &symbol_table.lookup(&i) {
            Some(_) => {
                let expr_type = type_table.lookup(&i).unwrap_or(Type::Unknown);

                TypedAstNode::Identifier {
                    name: i,
                    node_type: expr_type,
                    location: loc,
                }
            }
            None => TypedAstNode::Identifier {
                name: i,
                node_type: Type::Unknown,
                location: loc,
            },
        },
        AstNode::Grouping(expr, loc) => {
            let typecheck = typecheck(*expr, symbol_table, type_table, errors);
            let typed_expr = typecheck;
            let expr_type = typed_expr.get_type();
            TypedAstNode::Grouping {
                expr: Box::new(typed_expr),
                node_type: expr_type,
                location: loc,
            }
        }
        AstNode::Binary {
            op,
            lhs,
            rhs,
            location,
        } => {
            let typed_lhs = typecheck(*lhs, symbol_table, type_table, errors);
            let typed_rhs = typecheck(*rhs, symbol_table, type_table, errors);
            let l_type = typed_lhs.get_type();
            let r_type = typed_rhs.get_type();
            let (l_expected, r_expected) = allowed_binary_op_type(op, l_type.clone());

            let full_type = if (&l_expected, &r_expected) == (&l_type, &r_type) {
                binary_return_type(op, l_type, r_type)
            } else {
                let error = TypeError::new(
                    &format!(
                        "expected {l_expected} {op} {r_expected} \n got {l_type} {op} {r_type}"
                    ),
                    location,
                );
                errors.push(error);
                Type::Unknown
            };

            TypedAstNode::Binary {
                node_type: full_type,
                op,
                lhs: Box::new(typed_lhs),
                rhs: Box::new(typed_rhs),
                location,
            }
        }
        AstNode::Unary { op, expr, location } => {
            let typed_expr = typecheck(*expr, symbol_table, type_table, errors);
            let e_type = typed_expr.get_type();
            let expected_types = allowed_infix_op_type(op, e_type.clone());
            let mut node_type = expected_types[0].clone();
            if !expected_types.contains(&e_type) {
                let mut types_str = "".to_string();
                for i in expected_types {
                    let display = i.to_string() + ", ";
                    types_str.push_str(&display);
                }
                types_str = types_str.trim().trim_end_matches(',').to_string();
                let error = TypeError::new(
                    &format!("expected one of: {types_str} \n got {e_type}"),
                    location,
                );
                errors.push(error);
                node_type = Type::Unknown;
            }

            TypedAstNode::Unary {
                node_type,
                op,
                expr: Box::new(typed_expr),
                location,
            }
        }
        AstNode::If {
            condition,
            if_body,
            else_body,
            location,
        } => {
            let condition_typed = typecheck(*condition, symbol_table, type_table, errors);
            let condition_type = condition_typed.get_type();
            if condition_type != Type::Bool {
                let error = TypeError::new(
                    "expected a boolean value",
                    condition_typed.source_location(),
                );
                errors.push(error);
            }
            let if_body_typed = typecheck(*if_body, symbol_table, type_table, errors);
            let else_body_typed = typecheck(*else_body, symbol_table, type_table, errors);

            let full_type = if if_body_typed.get_type() == else_body_typed.get_type() {
                if_body_typed.get_type()
            } else {
                let error =
                    TypeError::new("return types of if and else blocks should match", location);
                errors.push(error);
                Type::Unknown
            };

            TypedAstNode::If {
                node_type: full_type,
                condition: Box::new(condition_typed),
                if_body: Box::new(if_body_typed),
                else_body: Box::new(else_body_typed),
                location,
            }
        }
        AstNode::VariableDeclaration {
            variable,
            value,
            location,
        } => {
            let val_node = typecheck(*value.clone(), symbol_table, type_table, errors);
            let val_type = val_node.get_type();
            type_table.insert(&variable, val_type);
            TypedAstNode::VariableDeclaration {
                variable,
                value: Box::new(val_node),
                node_type: Type::Unit,
                location,
            }
        }
        AstNode::Declarations(nodes, location) => {
            let mut declarations: Vec<TypedAstNode> = Vec::new();
            for node in nodes {
                declarations.push(typecheck(node, symbol_table, type_table, errors));
            }
            let node_types: Vec<Type> = declarations
                .clone()
                .into_iter()
                .map(|x| x.get_type())
                .collect();
            TypedAstNode::Declarations {
                declarations,
                node_type: Type::Declarations(node_types),
                location,
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

fn allowed_infix_op_type(operator: Operator, expr_type: Type) -> Vec<Type> {
    match operator {
        Operator::Negate => {
            let allowed_types = vec![Type::Int, Type::Float];
            if allowed_types.contains(&expr_type) {
                vec![expr_type]
            } else {
                allowed_types
            }
        }
        _ => vec![Type::Unknown],
    }
}
