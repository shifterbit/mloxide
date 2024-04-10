use std::{env, fs};
mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let text = fs::read_to_string(path);
    let tokens = lexer::Lexer::new(&text.unwrap()).tokens();
    for token in tokens {
        println!("{}", token)
    }
}



