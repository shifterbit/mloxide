use std::{env, fs};

use parser::parse;

use crate::{
    ast::{AstNode, Type, TypedAstNode},
    error_reporting::errors_from_file,
    interpreter::{eval_expression, Value},
    name_resolution::{resolve_symbols, SymbolTable},
    type_checker::typecheck,
};
mod ast;
mod error_reporting;
mod interpreter;
mod lexer;
mod name_resolution;
mod parser;
mod source_location;
mod token;
mod type_checker;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path).unwrap();
    println!("Plaintext:\n {:#?}", &text);
    let mut lexer = lexer::Lexer::new(&text);
    let ast = parse(&mut lexer);
    match ast {
        Ok(a) => {
            println!("AST:\n {:#?}", a);
            let mut symtable: SymbolTable<AstNode> = SymbolTable::new();
            resolve_symbols(a.clone(), &mut symtable);
            println!("SymbolTable:\n {:#?}", symtable);
            let mut type_table: SymbolTable<Type> = SymbolTable::new();
            let tast = typecheck(a.clone(), &symtable, &mut type_table);
            println!("Typed AST:\n {:#?}", tast);
            let mut value_table: SymbolTable<Value> = SymbolTable::new();
            let eval = eval_expression(tast, &mut value_table);
            println!("{:?}", eval);
        }
        Err(e) => {
            errors_from_file(path, &text, e.clone());
        }
    }
}
