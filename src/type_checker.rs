use crate::{
    ast::{AstNode, Operator, Type, TypedAstNode},
    name_resolution::SymbolTable, source_location::SourcePosition,
};


pub fn typecheck(
    node: AstNode,
    symbol_table: &SymbolTable<AstNode>,
    type_table: &mut SymbolTable<Type>,
) -> TypedAstNode {
    match node {
        AstNode::Error(loc) => TypedAstNode::Error(loc),
        AstNode::Int(n, loc) => TypedAstNode::Int(n, loc),
        AstNode::Float(n, loc) => TypedAstNode::Float(n, loc),
        AstNode::Bool(b, loc) => TypedAstNode::Bool(b, loc),
        AstNode::Identifier(i, loc) => match &symbol_table.lookup(&i) {
            Some(exp) => {
                let typed_expr = typecheck(exp.clone(), symbol_table, type_table);
                let expr_type = typed_expr.get_type();
                type_table.insert(&i, expr_type.clone());
                TypedAstNode::Identifier {
                    name: i,
                    node_type: expr_type,
                    location: loc
                }
            }
            None => TypedAstNode::Identifier {
                name: i,
                node_type: Type::Unknown,
                location: loc
            },
        },
        AstNode::Grouping(expr, loc) => {
            let typecheck = typecheck(*expr, symbol_table, type_table);
            let typed_expr = typecheck;
            let expr_type = typed_expr.get_type();
            TypedAstNode::Grouping {
                expr: Box::new(typed_expr),
                node_type: expr_type,
                location: loc
            }
        }
        AstNode::Binary { op, lhs, rhs, location } => {
            let typed_lhs = typecheck(*lhs, symbol_table, type_table);
            let typed_rhs = typecheck(*rhs, symbol_table, type_table);
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
                location
            }
        }
        AstNode::Unary { op, expr, location } => {
            let typed_expr = typecheck(*expr, symbol_table, type_table);
            let e_type = typed_expr.get_type();
            let expected_type = allowed_infix_op_type(op, e_type.clone());
            TypedAstNode::Unary {
                node_type: expected_type,
                op,
                expr: Box::new(typed_expr),
                location
            }
        }
        AstNode::If {
            condition,
            if_body,
            else_body,
            location
        } => {
            let condition_typed = typecheck(*condition, symbol_table, type_table);
            let if_body_typed = typecheck(*if_body, symbol_table, type_table);
            let else_body_typed = typecheck(*else_body, symbol_table, type_table);

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
                location
            }
        }
        AstNode::VariableDeclaration { variable, value , location} => {
            let val_node = typecheck(*value.clone(), symbol_table, type_table);
            let val_type = val_node.get_type();
            TypedAstNode::VariableDeclaration {
                variable,
                value: Box::new(val_node),
                node_type: val_type,
                location
            }
        }
        AstNode::Declarations(nodes, location) => {
            let mut declarations: Vec<TypedAstNode> = Vec::new();
            for node in nodes {
                declarations.push(typecheck(node, symbol_table, type_table));
            }
            let node_types: Vec<Type> = declarations
                .clone()
                .into_iter()
                .map(|x| x.get_type())
                .collect();
            TypedAstNode::Declarations {
                declarations,
                node_type: Type::Declarations(node_types),
                location
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
