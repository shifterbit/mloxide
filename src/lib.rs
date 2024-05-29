use std::fs;

use ast::{ASTNode, AnnotatedASTNode};
use error_reporting::errors_from_file;
use interpreter::Value;
use lexer::Lexer;
use parser::parse;
use symbol_table::{resolve_symbols, SymbolTable};
use type_checker::check;


pub mod ast;
pub mod error_reporting;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod pattern;
pub mod source_location;
pub mod symbol_table;
pub mod token;
pub mod type_checker;
pub mod types;

pub fn run_file(filepath: &str) {
    let source = fs::read_to_string(filepath).unwrap();
    let mut lexer = Lexer::new(&source);
    let ast = parse(&mut lexer);
    match ast {
        Ok(a) => {
            let mut annotated_ast = a.annotate();
            let mut symbol_table: SymbolTable<AnnotatedASTNode> = SymbolTable::new();
            resolve_symbols(&mut annotated_ast, &mut symbol_table);
            let typed_ast = check(annotated_ast, &symbol_table);
            match typed_ast {
                Ok(tast) => {
                    let mut value_table: SymbolTable<Value> = SymbolTable::new();
                    let result = interpreter::eval_expression(tast, &mut value_table);
                    println!("{}", result);
                }
                Err(errors) => {
                    errors_from_file(filepath, &source, errors);
                }
            }
        }
        Err((errors, _)) => {
            errors_from_file(filepath, &source, errors);
        }
    }
}

pub fn generate_ast(source: &str) -> ASTNode {
    let mut lexer = Lexer::new(source);
    let ast = parse(&mut lexer);
    match ast {
        Ok(a) => a,
        Err((_, a)) => a,
    }
}
