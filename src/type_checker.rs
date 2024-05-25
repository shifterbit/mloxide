use std::fmt::{self, Display};

use crate::{
    ast::{ASTNode, Operator, Type, TypedASTNode},
    error_reporting::CompilerError,
    source_location::{SourceLocation, SourcePosition},
    symbol_table::SymbolTable,
};

pub type TypeErrorList = Vec<TypeError>;
#[derive(Debug)]
pub struct TypeError {
    message: String,
    location: SourceLocation,
    details: Option<Vec<(String, SourceLocation)>>,
    explaination: Option<String>,
}

impl CompilerError for TypeError {
    fn new(
        message: &str,
        location: SourceLocation,
        details: Option<Vec<(String, SourceLocation)>>,
        explaination: Option<String>,
    ) -> TypeError {
        TypeError {
            message: message.to_string(),
            location,
            details,
            explaination,
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

    fn details(&self) -> Option<Vec<(String, SourceLocation)>> {
        self.details.clone()
    }
    fn explaination(&self) -> Option<String> {
        self.explaination.clone()
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn check(
    ast: ASTNode,
    symbol_table: &SymbolTable<ASTNode>,
) -> Result<TypedASTNode, TypeErrorList> {
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
    node: ASTNode,
    symbol_table: &SymbolTable<ASTNode>,
    type_table: &mut SymbolTable<Type>,
    errors: &mut TypeErrorList,
) -> TypedASTNode {
    match node {
        ASTNode::Error(loc) => TypedASTNode::Error(loc),
        ASTNode::Int(n, loc) => TypedASTNode::Int(n, loc),
        ASTNode::Float(n, loc) => TypedASTNode::Float(n, loc),
        ASTNode::Bool(b, loc) => TypedASTNode::Bool(b, loc),
        ASTNode::Identifier(i, loc) => match &symbol_table.lookup(&i) {
            Some(_) => {
                let expr_type = type_table.lookup(&i).unwrap_or(Type::Unknown);

                TypedASTNode::Identifier {
                    name: i,
                    node_type: expr_type,
                    location: loc,
                }
            }
            None => TypedASTNode::Identifier {
                name: i,
                node_type: Type::Unknown,
                location: loc,
            },
        },
        ASTNode::Grouping(expr, loc) => match expr {
            Some(exp) => {
                let typecheck = typecheck(*exp, symbol_table, type_table, errors);
                let typed_expr = typecheck;
                let expr_type = typed_expr.get_type();
                TypedASTNode::Grouping {
                    expr: Some(Box::new(typed_expr)),
                    node_type: expr_type,
                    location: loc,
                }
            }
            None => TypedASTNode::Grouping {
                expr: None,
                node_type: Type::Unit,
                location: loc,
            },
        },
        ASTNode::Binary {
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
                let details = Some(vec![
                    (format!("is of type {l_type}"), typed_lhs.source_location()),
                    (format!("is of type {r_type}"), typed_rhs.source_location()),
                ]);
                let error = TypeError::new(
                    "mismatched types",
                    location,
                    details,
                    Some(format!(
                        "expected {l_expected} {op} {r_expected},  got {l_type} {op} {r_type}"
                    )),
                );
                errors.push(error);
                Type::Unknown
            };

            TypedASTNode::Binary {
                node_type: full_type,
                op,
                lhs: Box::new(typed_lhs),
                rhs: Box::new(typed_rhs),
                location,
            }
        }
        ASTNode::Unary { op, expr, location } => {
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
                let labels = Some(vec![
                    (format!("is of type {e_type}"), typed_expr.source_location()),
                    (format!("expected one of: {types_str}"), location),
                ]);
                let error = TypeError::new(
                    &format!("value of type {e_type} cannot be used with operator {op}"),
                    location,
                    labels,
                    None,
                );
                errors.push(error);
                node_type = Type::Unknown;
            }

            TypedASTNode::Unary {
                node_type,
                op,
                expr: Box::new(typed_expr),
                location,
            }
        }
        ASTNode::If {
            condition,
            if_body,
            else_body,
            location,
        } => {
            let condition_typed = typecheck(*condition, symbol_table, type_table, errors);
            let condition_type = condition_typed.get_type();
            if condition_type != Type::Bool {
                let error = TypeError::new(
                    "expected a boolean value for the if condition",
                    condition_typed.source_location(),
                    None,
                    None,
                );
                errors.push(error);
            }
            let if_body_typed = typecheck(*if_body, symbol_table, type_table, errors);
            let else_body_typed = typecheck(*else_body, symbol_table, type_table, errors);

            let full_type = if if_body_typed.get_type() == else_body_typed.get_type() {
                if_body_typed.get_type()
            } else {
                let labels = Some(vec![
                    (
                        format!("This expression is of type {}", if_body_typed.get_type()),
                        if_body_typed.source_location(),
                    ),
                    (
                        format!("This expression is of type {}", else_body_typed.get_type()),
                        else_body_typed.source_location(),
                    ),
                ]);
                let error = TypeError::new(
                    "mismatched types",
                    location,
                    labels,
                    Some("Types of if and else blocks should match".to_string()),
                );
                errors.push(error);
                Type::Unknown
            };

            TypedASTNode::If {
                node_type: full_type,
                condition: Box::new(condition_typed),
                if_body: Box::new(if_body_typed),
                else_body: Box::new(else_body_typed),
                location,
            }
        }
        ASTNode::VariableDeclaration {
            variable,
            value,
            location,
        } => {
            let val_node = typecheck(*value.clone(), symbol_table, type_table, errors);
            let val_type = val_node.get_type();
            type_table.insert(&variable, val_type);
            TypedASTNode::VariableDeclaration {
                variable,
                value: Box::new(val_node),
                node_type: Type::Unit,
                location,
            }
        }
        ASTNode::Declarations(nodes, location) => {
            let mut declarations: Vec<TypedASTNode> = Vec::new();
            for node in nodes {
                let declaration = typecheck(node, symbol_table, type_table, errors);
                declarations.push(declaration);
            }
            let node_types: Vec<Type> = declarations
                .clone()
                .into_iter()
                .map(|x| x.get_type())
                .collect();
            TypedASTNode::Declarations {
                declarations,
                node_type: Type::Declarations(node_types),
                location,
            }
        }
        ASTNode::Tuple(nodes, location) => {
            let mut exprs: Vec<TypedASTNode> = Vec::new();
            for node in nodes {
                let expr = typecheck(node, symbol_table, type_table, errors);
                exprs.push(expr);
            }
            let node_types: Vec<Type> = exprs
                .clone()
                .into_iter()
                .map(|x| x.get_type())
                .collect();
            TypedASTNode::Tuple {
                exprs,
                node_type: Type::Tuple(node_types),
                location,
            }
        }
        ASTNode::Let {
            declarations,
            expr,
            location,
            environment,
        } => {
            let mut scoped_type_table = type_table.enter_scope();
            let mut typed_declarations: Vec<TypedASTNode> = Vec::new();
            for declaration in declarations {
                let typed_declaration = typecheck(
                    declaration,
                    environment.as_ref().unwrap(),
                    &mut scoped_type_table,
                    errors,
                );
                typed_declarations.push(typed_declaration);
            }

            let typed_expr = typecheck(
                *expr,
                environment.as_ref().unwrap(),
                &mut scoped_type_table,
                errors,
            );
            let full_type = typed_expr.get_type();

            TypedASTNode::Let {
                declarations: typed_declarations,
                expr: Box::new(typed_expr),
                location,
                environment: Some(Box::new(scoped_type_table)),
                node_type: full_type,
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
        Operator::Equal
        | Operator::NotEqual
        | Operator::LessThan
        | Operator::LessEqual
        | Operator::GreaterEqual
        | Operator::GreaterThan => Type::Bool,
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
        Operator::Equal
        | Operator::NotEqual
        | Operator::LessThan
        | Operator::LessEqual
        | Operator::GreaterEqual
        | Operator::GreaterThan => {
            vec![Type::Bool]
        }
    }
}
fn allowed_binary_op_type(operator: Operator, left_type: Type) -> (Type, Type) {
    match operator {
        Operator::Add
        | Operator::Subtract
        | Operator::Multiply
        | Operator::Divide
        | Operator::LessThan
        | Operator::LessEqual
        | Operator::GreaterEqual
        | Operator::GreaterThan => match left_type {
            Type::Int => (Type::Int, Type::Int),
            Type::Float => (Type::Float, Type::Float),
            _ => (Type::Unknown, Type::Unknown),
        },
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
