use std::{env, fs};

use parser::parse;

use crate::{interpreter::eval_expression, parser::stringify_parse_errors, type_checker::typecheck};
mod ast;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod type_checker;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path);
    println!("Plaintext:\n {:#?}", &text);
    let mut lexer = lexer::Lexer::new(&text.unwrap());
    let ast = parse(&mut lexer);
    match ast {
        Ok(a) => {
            println!("AST:\n {:#?}", a);
            let tast = typecheck(a);
            println!("Typed AST:\n {:#?}", tast);
            let eval = eval_expression(tast);
            println!("Eval Result:\n {:#?}", eval);
        }
        Err(e) => {
            let errors = stringify_parse_errors(e);
            for error in errors {
                println!("{}", error);
            }
        }
    }

}
