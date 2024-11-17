use std::{fs, process::exit};

use ast::ASTNode;
use error_reporting::errors_from_file;
use interpreter::{eval, Value};
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
    let mut ast: ASTNode = parse(&mut lexer).unwrap_or_else(|(errors,_)| {
        errors_from_file(filepath, &source, errors);
        exit(1);
    });
    let sym_table: SymbolTable<ASTNode> = resolve_symbols(&mut ast).unwrap_or_else(|errors| {
        errors_from_file(filepath, &source, errors);
        exit(1);
    });
    let typed_ast = check(ast, &sym_table).unwrap_or_else(|errors| {
        errors_from_file(filepath, &source, errors);
        exit(1);
    });
    let result = eval(typed_ast);
    println!("{}", result);
}

pub fn generate_ast(source: &str) -> ASTNode {
    let mut lexer = Lexer::new(source);
    let ast = parse(&mut lexer);
    match ast {
        Ok(a) => a,
        Err((_, a)) => a,
    }
}
