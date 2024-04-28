use std::{env, fs};

use parser::parse;

use crate::{interpreter::eval_expression, type_checker::typecheck};
mod ast;
mod lexer;
mod parser;
mod type_checker;
mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path);
    println!("Plaintext:\n {:#?}", &text);
    let mut lexer = lexer::Lexer::new(&text.unwrap());
    println!("Lexer:\n {:#?}", &lexer);
    let ast = parse(&mut lexer);
    println!("AST:\n {:#?}", ast);
    let tast = typecheck(Box::new(ast));
    println!("Typed AST:\n {:#?}", tast);
    let eval = eval_expression(tast);
    println!("Eval Result:\n {:#?}", eval);
}
