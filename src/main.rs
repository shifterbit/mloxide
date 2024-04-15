use std::{env, fs};

use parser::parse;
mod lexer;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path);
    let mut lexer = lexer::Lexer::new(&text.unwrap());
    let ast = parse(&mut lexer);
    println!("{:#?}", ast);
    
}



