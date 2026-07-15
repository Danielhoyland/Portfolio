// Import necessary modules
use std::str::FromStr;
use crate::arithemetics::{calculate,check_if_math};
use crate::bool::{combine_bool, capitalize, check_if_bool};
use crate::comparison::{check_if_compare,compare_values};
use crate::stack_operations::{check_if_stack_op, op_stack};
use crate::length::{length_op};

// Define a function that takes a string and returns a vector of its whitespace-separated tokens
pub fn tokenize_input(input: &str) -> Vec<&str> {
    input.trim().split_whitespace().collect()
}

//did not implement this stack saving yet so nothing right now is save but you have to 
//input everything you want into it at the same time
pub fn tokens_to_stack(tokens: &[&str]) -> Vec<String> {
    let mut stack: Vec<String> = Vec::new();
    for token in tokens {
        match *token {
            "[" => stack.push("[".to_string()),
            "]" => {
                let mut list: Vec<String> = Vec::new();
                while stack.last().unwrap() != "[" {
                    let item = stack.pop().unwrap();
                    list.push(item);
                }
                list.reverse();
                stack.pop();
                stack.push(format!("[{}]", list.join(" "))); // use space as separator
            }
            _ => stack.push(token.to_string()),
        }
    }
    stack
}
// This function takes an array of string references as input and returns a single string as output.
pub fn format_input_tokens(tokens: &[&str]) -> String {

    // Initialize various boolean flags and counters to track the state of the function
    let mut result = String::new(); // The final output string that will be returned
    let mut in_brackets = false; // Whether the current token is inside square brackets []
    let mut bracket_num = 0; // Keeps track of how many open square brackets there are
    let mut in_curly_bracket = false; // Whether the current token is inside curly braces {}
    let mut in_quote = false; // Whether the current token is inside quotes ""
    let mut in_bool = false; // Whether the current token is a boolean value
    let mut ignore_program = 0; // Used to ignore tokens that have already been processed
    let mut num_removed = 0; // Keeps track of how many tokens have been ignored so far
    let mut multi_op_bool = false; // Whether the current token is a multi-operator boolean expression
    let mut multi_op_string = String::new(); // Used to construct a string of multiple boolean operators

    // Loop through each token in the input array
    for (i, token) in tokens.iter().enumerate() {

        // Ignore tokens that have already been processed
        if ignore_program < (i+1) {

            // Check if the current token is a curly brace
            if <&str as AsRef<str>>::as_ref(token) == "{" {
                in_curly_bracket = true;
            } else if <&str as AsRef<str>>::as_ref(token) == "}" {
                in_curly_bracket = false;
            }

            // Check if the current token is a quote
            {
                if <&str as AsRef<str>>::as_ref(token) == "\"" && in_quote == false {
                    in_quote = true;
                } else if <&str as AsRef<str>>::as_ref(token) == "\"" && in_quote == true {
                    in_quote = false;
                }
            }

            // Check if the current token is inside square brackets
            if !in_curly_bracket && !in_quote {
                if <&str as AsRef<str>>::as_ref(token) == "[" {
                    bracket_num += 1;
                } else if <&str as AsRef<str>>::as_ref(token) == "]" {
                    bracket_num -= 1;
                }

                // Check if the current token is inside one or more square brackets
                if bracket_num > 0 {
                    in_brackets = true;
                } else if bracket_num == 0 {
                    in_brackets = false;
                }

                // Check if the current token contains a space
                if <&str as AsRef<str>>::as_ref(token).contains(' ') {

                    // If the current token is a string containing no spaces, remove the quotes and add it to the output
                    if <&str as AsRef<str>>::as_ref(token).starts_with("\"") && <&str as AsRef<str>>::as_ref(token).ends_with("\"") && !<&str as AsRef<str>>::as_ref(token)[1..(token.len()-1)].contains(' ') {
                        result.push_str(&format!("{}",&<&str as AsRef<str>>::as_ref(token)[1..(token.len()-1)]));
                    } else {
                        // Otherwise, add quotes around the token and add it to the output
                        result.push_str(&format!("\"{}\"", token));
                    }
                } else if check_if_math(token) {
                    // If the token is a mathematical operator, remove the last item from the result vector
                    result.pop();
                
                    // Create a new string to store the second number in the mathematical operation
                    let mut num2 = String::new();
                
                    // Set flags for decimal points
                    let mut has_decimal = false;
                    let mut result_has_decimal = false;
                
                    // Iterate through the result vector in reverse until we reach a space or comma
                    while let Some(char_value) = result.pop() {
                        if char_value == ' ' || char_value == ',' {
                            break;
                        }
                
                        // If the character is '0' and the number is empty, add it to the string
                        else if char_value == '0' && num2.is_empty() {
                            num2.push('0');
                        }
                
                        // If the character is a digit, add it to the string
                        else if let Some(digit) = char_value.to_digit(10) {
                            // If we've already encountered a decimal point, add it to the string with a decimal
                            if has_decimal {
                                num2.push('.');
                                num2.push((digit as u8 + b'0') as char);
                                has_decimal = false;
                            }
                            // Otherwise, add the digit to the string
                            else {
                                num2.push((digit as u8 + b'0') as char);
                            }
                        }
                
                        // If the character is a decimal point, set the flag for the second number and the result number
                        else if char_value == '.' {
                            has_decimal = true;
                            result_has_decimal = true;
                        }
                    }
                                
                // Reverse the order of characters in `num2` and convert it to a float
                let num2 = num2.chars().rev().collect::<String>();
                let num2 = num2.parse::<f64>().unwrap();

                // Initialize `num1` and a flag to check if it contains a decimal point
                let mut num1 = String::new();
                let mut has_decimal = false;

                // Pop characters from `result` until a space or comma is found, then append
                // them to `num1` in reverse order. If a decimal point is encountered, set
                // the `has_decimal` flag.
                while let Some(char_value) = result.pop() {
                    if char_value == ' ' || char_value == ',' {
                        break;
                    }
                    if char_value == '0' && num1.is_empty() {
                        num1.push('0');
                    } else if let Some(digit) = char_value.to_digit(10) {
                        if has_decimal {
                            num1.push('.');
                            num1.push((digit as u8 + b'0') as char);
                            has_decimal = false;
                        } else {
                            num1.push((digit as u8 + b'0') as char);
                        }
                    } else if char_value == '.' {
                        has_decimal = true;
                        result_has_decimal = true;
                    }
                }

                // Reverse the order of characters in `num1` and convert it to a float
                let num1 = num1.chars().rev().collect::<String>();
                let num1 = num1.parse::<f64>().unwrap();

                // Calculate the result of the operation and convert it to a string
                let resulting_num = calculate(token, num1, num2).to_string();

                // Check if the resulting string contains a decimal point and set the
                // `result_contain_dec` flag accordingly
                let mut result_contain_dec = false;
                if resulting_num.contains(".") {
                    result_contain_dec = true;
                }
                if (token == &"+" || token == &"-" || token == &"*") && (num1.fract() != 0.0 || num2.fract() != 0.0 || result_has_decimal) && !result_contain_dec {
                    // Check if the token is one of the arithmetic operators and if either number is a float or the result has a decimal point
                    // If true, add a decimal point to the resulting value
                    if !result.ends_with(',') && !result.is_empty(){
                        // Check if the result already has a comma
                        result.push_str(",");
                    }
                    result.push_str(&resulting_num.replace(".0", ""));
                    result.push_str(".0");
                } else if token == &"/" && (resulting_num.ends_with("0") || resulting_num == "0") {
                    // Check if the token is a division operator and the result ends with a 0 or equals to 0
                    let mut num_with_decimal = resulting_num.clone();
                    if resulting_num.starts_with("-") {
                        // If the result is negative, remove the negative sign from num_with_decimal and add it to the result
                        num_with_decimal.remove(0);
                        result.push('-');
                    }
                    result.push_str(num_with_decimal.as_str());
                    result.push_str(".0");
                } else {
                    // If none of the above conditions is true, add the resulting value as is to the result string
                    if !result.ends_with(',') && !result.is_empty(){
                        // Check if the result already has a comma
                        result.push_str(",");
                    }
                    result.push_str(resulting_num.as_str());
                }
                
            }else if i + 1 < tokens.len() && (token == &"True" || token == &"False") && !in_brackets && (if i+2 < tokens.len() {check_if_bool(tokens[i+2])}else{check_if_bool(tokens[i+1])}) {
                // Check if the current token is "True" or "False", the next token is a boolean operator, and we're not currently in brackets
                in_bool = true;
                if i + 2 < tokens.len() && (<&str as AsRef<str>>::as_ref(&tokens[i + 1]) == "True" || <&str as AsRef<str>>::as_ref(&tokens[i + 1]) == "False")&& (<&str as AsRef<str>>::as_ref(&tokens[i + 2]) == "&&" || <&str as AsRef<str>>::as_ref(&tokens[i + 2]) == "||") {
                    // If the next two tokens are a boolean value and a boolean operator, combine them with the current boolean value
                    result.push_str(capitalize(combine_bool(
                        bool::from_str(token.to_lowercase().as_str()).unwrap(),
                        &tokens[i + 2],
                        Some(bool::from_str(&tokens[i + 1].to_string().to_lowercase().as_str()).unwrap())
                    ).to_string().to_uppercase().as_str()).as_str());
                    ignore_program = i + 2 + 1; // Skip over the next two tokens and the current token
                } 
                else {
                    // Otherwise, combine the current boolean value with the next token
                    match bool::from_str(token.to_lowercase().as_str()) {
                        Ok(bool_val) => {
                            result.push_str(
                                capitalize(combine_bool(bool_val, &tokens[i + 1], None).to_string().as_str()).as_str(),
                            );
                            ignore_program = i + 1 + 1; // Skip over the next token and the current token
                        }
                        Err(err) => {
                            // handle the error
                            eprintln!("Error parsing Boolean value from token '{}': {}", token, err);
                            // continue to the next token
                            ignore_program = i;
                        }
                    }
                }                             
            }
            
                // This is an else-if statement that checks if the current token is a comparison operator and the index of the previous token is greater than or equal to 0.
            else if check_if_compare(token) && (i-2) >= 0 {
                // Initialize some variables for counting brackets and reversing the order of brackets.
                let mut backward_bracket_num = 0;
                let mut reversed_brackets1 = String::new();
                let mut reversed_brackets2 = String::new();
                // If the token immediately to the left is ']', search backwards for the corresponding '['.
                if tokens[i-1] == "]" {
                    let mut k = 0;
                    reversed_brackets1.push(']');
                    backward_bracket_num += 1;
                    for j in (0..(i-1)).rev() {
                        if backward_bracket_num == 0 {
                            k = j;
                            break;
                        }
                        else if tokens[j] == "]" {
                            backward_bracket_num += 1;
                            reversed_brackets1.push(']');
                        } else if tokens[j] == "[" {
                            backward_bracket_num -= 1;
                            reversed_brackets1.push('[');
                        } else {
                            reversed_brackets1.push(tokens[j].chars().next().unwrap());
                        }  
                    }
                    // Reverse the order of the brackets to get the original order.
                    let brackets1 = reversed_brackets1.chars().rev().collect::<String>();
                    
                    if tokens[k] == "]"{
                        reversed_brackets2.push(']');
                        backward_bracket_num += 1;
                        for j in (0..k-1).rev() {
                            if backward_bracket_num == 0 {
                                break;
                            }
                            else if tokens[j] == "]" {
                                backward_bracket_num += 1;
                                reversed_brackets2.push(']');
                            } else if tokens[j] == "[" {
                                backward_bracket_num -= 1;
                                reversed_brackets2.push('[');
                            }
                                else {
                                reversed_brackets2.push(tokens[j].chars().next().unwrap());
                            }  
                        }
                        // reverse the second set of brackets to get the correct order
                        let brackets2 = reversed_brackets1.chars().rev().collect::<String>();
                        // calculate the total length of the string between the two brackets, including the comparison token
                        let total_length = brackets2.len() + brackets1.len() + token.len();
                        //removes everything for result (it is still saved as tokens)
                        for _i in 0..total_length {
                            result.pop();
                        }
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        //gets the values from token itno compare values 
                        result.push_str(capitalize(compare_values(&brackets2, &brackets1, token).to_string().as_str()).as_str());
                    } else {
                        //gets total length and removes that amount of characters from result
                        let total_length = tokens[k].len() + brackets1.len() + token.len();
                        for _i in 0..total_length {
                            result.pop();
                        }
                        result.push_str(capitalize(compare_values(&tokens[k], &brackets1, token).to_string().as_str()).as_str());
                        
                    }
                    //this gets activated if the first value wasnt a bracket but the second is
                } else if tokens[i-2] == "]" {
                    let mut k = 0;
                    reversed_brackets1.push(']');
                    backward_bracket_num += 1;
                    for j in (0..i-1).rev() {
                        // Find the corresponding opening bracket and count backward
                        if tokens[j] == "]" {
                            backward_bracket_num += 1;
                            reversed_brackets1.push(']');
                        } else if tokens[j] == "[" {
                            backward_bracket_num -= 1;
                            reversed_brackets1.push('[');
                        } else if backward_bracket_num == 0 {
                            k = j;
                            break;
                        } else {
                            reversed_brackets1.push(tokens[j].chars().next().unwrap());
                        }
                    }
                    let brackets1 = reversed_brackets1.chars().rev().collect::<String>();
                    // Calculate the total length of the difference between the values and delete that many characters from the string
                    let total_length = tokens[k-1].len() + brackets1.len() + token.len();
                    for _i in 0..total_length {
                        result.pop();
                    }
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.push_str(capitalize(compare_values(&tokens[k-1], &brackets1, token).to_string().as_str()).as_str());
                } // if it's a "" it's protected
                else if tokens[i-1] == "\"" {
                    let mut reversed_quotes1 = String::new();
                    let mut reversed_quotes2 = String::new();
                    let mut k=0;
                    reversed_quotes1.push('\"');
                    for j in (0..(i-1)).rev() {
                        // Find the corresponding opening quotes
                        if tokens[j] == "\"" {
                            reversed_quotes1.push('\"');
                            k=j;
                            break;
                        }
                        else {
                            reversed_quotes1.push(tokens[j].chars().next().unwrap());
                        }  
                    }
                    let reversed_quotes1 = reversed_quotes1.chars().rev().collect::<String>();
                    if tokens[k] == "\"" {
                        reversed_quotes2.push('\"');
                        for j in (0..(k-1)).rev() {
                            // Find the corresponding opening quotes
                            if tokens[j] == "\"" {
                                reversed_quotes2.push('\"');
                                k=j;
                                break;
                            }
                            else {
                                reversed_quotes2.push(tokens[j].chars().next().unwrap());
                            }  
                        }
                        let reversed_quotes2 = reversed_quotes2.chars().rev().collect::<String>();
                        let total_length = reversed_quotes2.len() + reversed_quotes1.len() + token.len();
                        for _i in 0..total_length {
                            result.pop();
                        }
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.push_str(capitalize(compare_values(&reversed_quotes2, &reversed_quotes1, token).to_string().as_str()).as_str());
                    }
                    else{
                        let total_length = tokens[k-1].len() + reversed_quotes1.len() + token.len();
                        for _i in 0..total_length {
                            result.pop();
                        }
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.pop();
                        result.push_str(capitalize(compare_values(tokens[k-1], &reversed_quotes1, token).to_string().as_str()).as_str());
                    }
                }                    
                // If the current token is a closing double quote
                else if tokens[i-1] == "\"" {
                    // Initialize a new string to hold the reversed string between the two quotes
                    let mut reversed_quotes2 = String::new();
                    reversed_quotes2.push('\"');
                    // Loop backwards through the tokens from the current position to find the opening double quote
                    for j in (0..(i-1)).rev() {
                        if tokens[j] == "\"" {
                            reversed_quotes2.push('\"');
                            break;
                        }
                        else {
                            reversed_quotes2.push(tokens[j].chars().next().unwrap());
                        }  
                    }
                    // Reverse the string and add it to the string between the quotes to get the final string
                    let reversed_quotes2 = reversed_quotes2.chars().rev().collect::<String>();
                    let total_length = reversed_quotes2.len() + tokens[i-1].len() + token.len();
                    // Remove the appropriate number of characters from the result string to get rid of the old values
                    for _i in 0..total_length {
                        result.pop();
                    }
                    // Remove the quotes and add the new value to the result string
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.push_str(capitalize(compare_values(&reversed_quotes2, tokens[i-1], token).to_string().as_str()).as_str());
                }
                // Check if multiple operations need to be performed
                else if multi_op_bool {
                    // Calculate the total length of the tokens involved
                    let total_length = tokens[i-2-num_removed].len() + tokens[i-1-num_removed].len() + token.len();
                    // Remove the tokens involved from the result string
                    for _i in 0..total_length {
                        result.pop();
                    }
                    // Remove the extra operators from the result string
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    // Compare the values of the two tokens and the operator
                    multi_op_string = compare_values(tokens[i-2-num_removed].to_lowercase().as_str(), &multi_op_string, token);
                    // Add the result of the comparison to the result string, after capitalizing it
                    result.push_str(capitalize(multi_op_string.to_string().as_str()).as_str());
                    // Debugging print statements
                    println!("{}",tokens[i-2-num_removed]);
                    println!("{}",multi_op_string);
                    println!("{}",num_removed);
                    // Increase the number of removed tokens by 2
                    num_removed +=2;
                } 
                else {
                    // Calculate the total length of the tokens involved
                    let total_length = tokens[i-2-num_removed].len() + tokens[i-1-num_removed].len() + token.len();
                    // Remove the tokens involved from the result string
                    for _i in 0..total_length {
                        result.pop();
                    }
                    // Remove the extra operators from the result string
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    result.pop();
                    // Compare the values of the two tokens and the operator
                    multi_op_string = compare_values(tokens[i-2-num_removed], tokens[i-1-num_removed], token);
                    // Add the result of the comparison to the result string, after capitalizing it
                    result.push_str(capitalize(multi_op_string.to_string().as_str()).as_str());
                    // Debugging print statements
                    println!("{}",tokens[i-2-num_removed]);
                    println!("{}",tokens[i-1-num_removed]);
                    println!("{}",num_removed);
                    // Increase the number of removed tokens by 2
                    num_removed +=2;
                    // Set the flag indicating multiple operations to true
                    multi_op_bool = true;
                }

            }
                // Check if the current token is a stack operation
                else if check_if_stack_op(token) {
                    // If it is, apply the operation to the result string using the op_stack function
                    result = op_stack(token, result.as_str())
                } 
                // Check if the current token is the "length" operator
                else if token == &"length"{
                    // If it is, apply the length_op function to the result string and convert the result to a string
                    result = length_op(&result).to_string();
                }          
                else {
                    // If the token is not a stack operation or the "length" operator, append it to the result string
                    result.push_str(token);
                }
                // Check if there is another token in the tokens list
                if i < tokens.len() - 1 {
                    let next_token = tokens[i + 1];
                    // Check if the next token is not a "]" or "[" and the current token is not in a Boolean context
                    if <&str as AsRef<str>>::as_ref(&next_token) != "]" && <&str as AsRef<str>>::as_ref(&token) != "[" && !in_bool{
                        // If so, append a comma to the result string
                        result.push(',');
                    }
                }
            } 
            else {
                // If not a special case, continue with normal behavior
                if i < tokens.len() - 1 {
                    // If not the last token
                    let next_token = tokens[i + 1];
                    if <&str as AsRef<str>>::as_ref(&next_token)== "\""{
                        // If the next token is a quotation mark
                        result.push_str(token);
                    }else if <&str as AsRef<str>>::as_ref(token)== "\""{
                        // If the current token is a quotation mark
                        result.push_str(token);
                    }else{
                        // If not a quotation mark, add token to result string with a space
                        result.push_str(token);
                        result.push_str(" ");
                    }
                }else{
                    // If it is the last token, add it to result string
                    if <&str as AsRef<str>>::as_ref(token)== "\""{
                        result.push_str(token);
                    }else{
                        result.push_str(token);
                        result.push_str(" ");
                    }
                }
            }            
        }
    }
    result
}