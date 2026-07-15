
mod functions;
mod arithemetics;
mod bool;
mod comparison;
mod stack_operations;
mod length;

use crate::functions::{tokenize_input,format_input_tokens};

pub fn t(input: &str) -> String {
    let tokens = tokenize_input(&input);

    format_input_tokens(&tokens)
}

