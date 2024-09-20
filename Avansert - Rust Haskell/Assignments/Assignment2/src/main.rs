use std::io::{self, Write};

mod functions;
mod arithemetics;
mod bool;
mod comparison;
mod stack_operations;
mod length;

use crate::functions::{tokenize_input,format_input_tokens};
fn main() {
    let mut stack: Vec<i32> = vec![];

    loop {
        print!("bprog> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let tokens = tokenize_input(&input);
        if tokens.is_empty() {
            println!("No input provided.");
            continue;
        }


        println!("{}", format_input_tokens(&tokens))
    }
}

