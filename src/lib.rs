use std::fs;

use ast::ASTNode;
use error_reporting::errors_from_file;
use interpreter::Value;
use lexer::Lexer;
use parser::parse;
use symbol_table::{resolve_symbols, SymbolTable};
use type_checker::check;

pub mod ast;
pub mod compiler;
pub mod error_reporting;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod source_location;
pub mod symbol_table;
pub mod token;
pub mod type_checker;
pub mod types;

pub fn run_file(filepath: &str) {
    let source = fs::read_to_string(filepath).unwrap();
    let mut lexer = Lexer::new(&source);
    match parse(&mut lexer) {
        Ok(ref mut ast) => {
            match resolve_symbols(ast) {
                Ok(sym_table) => match check(ast.clone(), &sym_table) {
                    Ok(tast) => {
                        let mut value_table: SymbolTable<Value> = SymbolTable::new();
                        let result = interpreter::eval_expression(tast.clone(), &mut value_table);
                        compiler::compile(tast);
                        println!("{}", result);
                    }
                    Err(errors) => {
                        errors_from_file(filepath, &source, errors);
                    }
                },
                Err(name_errors) => {
                    errors_from_file(filepath, &source, name_errors);
                }
            }
        }
        Err((errors, _ast)) => {
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
