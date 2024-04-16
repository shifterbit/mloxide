use std::{env, fs};

use parser::parse;

use crate::type_checker::typecheck;
mod ast;
mod lexer;
mod parser;
mod type_checker;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path);
    let mut lexer = lexer::Lexer::new(&text.unwrap());
    let ast = parse(&mut lexer);
    println!("{:#?}", ast);
    let tast = typecheck(Box::new(ast));
    println!("{:#?}", tast);
}
