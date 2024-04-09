use std::{env, fs};
mod lexer;

fn main() {
    println!("Hello, world!");
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let text = fs::read_to_string(file_path);
    let tokens = lexer::tokenize(text.unwrap());
    println!("{}", tokens[0])
}



