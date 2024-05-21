use std::env;

extern crate mloxide;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    mloxide::run(path);
}
