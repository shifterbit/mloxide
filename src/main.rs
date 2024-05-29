use std::env;

extern crate mloxide;

fn main() {
    let args: Vec<String> = env::args().collect();
    let name = &args[0];
    if args.len() < 2 {
        println!("Usage: {name} <path to file>")
    } else {
        mloxide::run_file(&args[1]);
    }
}
