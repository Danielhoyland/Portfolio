// This function checks if a given operator is a stack operator or not
pub fn check_if_stack_op(operator: &str) -> bool {
    match operator {
        "dup" => true,  // Returns true if the operator is "dup"
        "swap" => true, // Returns true if the operator is "swap"
        "pop" => true,  // Returns true if the operator is "pop"
        _ => false,     // Returns false for all other operators
    }
}

// This function performs the given stack operator on the stack provided
pub fn op_stack(operator: &str, stack: &str) -> String {
    println!("{}", stack); // Prints the current stack to the console

    // Parses the input string stack into a vector of i32 numbers
    let mut numbers: Vec<i32> = stack
        .split(",")
        .filter(|s| !s.is_empty())
        .map(|s| s.trim().parse().unwrap())
        .collect();

    // Matches the operator provided and performs the corresponding operation on the stack
    match operator {
        "dup" => {
            let last_idx = numbers.len() - 1;
            numbers.push(numbers[last_idx]); // Duplicates the last element of the stack
            numbers
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<String>>()
                .join(",") // Converts the stack back to a comma-separated string
        }
        "swap" => {
            if numbers.len() < 2 {
                return "Not enough elements in stack".to_string(); // Returns an error message if there are not enough elements in the stack to perform "swap"
            }
            let last_idx = numbers.len() - 1;
            let second_last_idx = last_idx - 1;
            numbers.swap(last_idx, second_last_idx); // Swaps the last two elements of the stack
            numbers
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<String>>()
                .join(",") // Converts the stack back to a comma-separated string
        }
        "pop" => {
            numbers.pop(); // Removes the last element of the stack
            numbers
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<String>>()
                .join(",") // Converts the stack back to a comma-separated string
        }
        _ => format!("Unknown operator: {}", operator), // Returns an error message for unknown operators
    }
}
