pub fn check_if_bool(operator: &str) -> bool {
    // Check if the given operator is a boolean operator
    match operator {
        "&&" => true,
        "||" => true,
        "not" => true,
        _ => false,
    }
}

pub fn combine_bool(a: bool, operator: &str, b: Option<bool>) -> bool {
    // Combine two boolean values using the given operator
    match operator {
        "&&" => match b {
            // If a second value is provided, return the logical AND of the two values
            Some(val) => a && val,
            // If a second value is not provided, return the first value
            None => a,
        },
        "||" => match b {
            // If a second value is provided, return the logical OR of the two values
            Some(val) => a || val,
            // If a second value is not provided, return the first value
            None => a,
        },
        "not" => match b {
            // If a second value is provided, panic because 'not' should only have one argument
            Some(_) => panic!("Too many arguments for operator 'not'"),
            // If a second value is not provided, return the logical NOT of the first value
            None => !a,
        },
        _ => panic!("Invalid operator: {}", operator),
    }
}

pub fn capitalize(word: &str) -> String {
    // Capitalize the first letter of the given word
    let mut chars = word.chars();
    if let Some(first_char) = chars.next() {
        let capitalized_first_char = first_char.to_uppercase();
        let rest_of_word = chars.as_str().to_lowercase();
        // Return the capitalized first letter concatenated with the rest of the word in lowercase
        return capitalized_first_char.to_string() + &rest_of_word;
    }
    // If the word is empty, return the empty string
    word.to_owned()
}
