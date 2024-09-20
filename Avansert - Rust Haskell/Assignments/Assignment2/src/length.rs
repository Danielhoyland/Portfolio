pub fn length_op(s: &str) -> usize {
    // Initialize variables
    let mut count: usize = 0; // to keep track of length
    let mut stack: Vec<char> = Vec::new(); // to keep track of brackets and braces
    let mut bracket_num: i32 = 0; // to keep track of nested brackets
    let mut brace_num: i32 = 0; // to keep track of nested braces

    for c in s.chars() {
        match c {
            '[' => { // start of a bracket
                bracket_num += 1; // increase count of nested brackets
                stack.push(c); // push the opening bracket onto the stack
            }
            ']' => { // end of a bracket
                bracket_num -= 1; // decrease count of nested brackets
                if let Some(opening_bracket) = stack.pop() { // get the last opening bracket from the stack
                    if opening_bracket == '[' && bracket_num == 0 { // if this is the last closing bracket for this opening bracket
                        count += 1; // increment the length counter
                    }
                }
            }
            '{' => { // start of a brace
                brace_num += 1; // increase count of nested braces
                stack.push(c); // push the opening brace onto the stack
            }
            '}' => { // end of a brace
                brace_num -= 1; // decrease count of nested braces
                count -= 1; // decrease the length counter (for each closing brace encountered)
            }
            '"' => { // start or end of a string
                while let Some(c) = s.chars().next() { // read the characters until the next double-quote is found
                    if c == '"' {
                        break;
                    }
                }
            }
            ',' => { // delimiter inside a bracket or a brace
                count += if bracket_num > 0 || brace_num > 0 { 1 } else { 0 }; // increment the length counter if we are inside a bracket or a brace
            }
            _ => { // any other character
                count += if bracket_num > 0 && c == ',' { 1 } // increment the length counter if inside a bracket and the character is a comma
                         else if bracket_num > 0 { 0 } // ignore other characters if inside a bracket
                         else if brace_num > 0 && c == ' ' { 1 } // increment the length counter if inside a brace and the character is a space
                         else if brace_num > 0 { 0 } // ignore other characters if inside a brace
                         else { 1 }; // increment the length counter for any other character
            }
        }
    }

    count // return the final length count
}
