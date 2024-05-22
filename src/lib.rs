use std::fs;

use ast::ASTNode;
use error_reporting::errors_from_file;
use interpreter::Value;
use lexer::Lexer;
use parser::parse;
use symbol_table::{resolve_symbols, SymbolTable};
use type_checker::check_types;

pub mod ast;
pub mod error_reporting;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod source_location;
pub mod symbol_table;
pub mod token;
pub mod type_checker;

pub fn run(filepath: &str) {
    let source = fs::read_to_string(filepath).unwrap();
    let mut lexer = Lexer::new(&source);
    let mut ast = parse(&mut lexer);
    match ast {
        Ok(ref mut a) => {
            let mut symbol_table: SymbolTable<ASTNode> = SymbolTable::new();
            resolve_symbols(a, &mut symbol_table);
            let typed_ast = check_types(a.clone(), &symbol_table);
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
        Err(errors) => {
            errors_from_file(filepath, &source, errors);
        }
    }
}
