// This function checks whether an operator is a comparison operator
pub fn check_if_compare(operator: &str) -> bool {
    match operator {
        "==" => true,
        "<" => true,
        ">" => true,
        _ => false,
    }
}

// This function compares two values using the specified operator and returns the result as a string
pub fn compare_values(value1: &str, value2: &str, operator: &str) -> String {
    match operator {
        "==" => {
            // Check if the values are numbers, and compare them as numbers if they are, otherwise compare as strings
            if is_number(value1) && is_number(value2) {
                (value1.parse::<f64>().unwrap() == value2.parse::<f64>().unwrap()).to_string()
            } else {
                (value1 == value2).to_string()
            }
        }
        "<" => {
            // Check if the values are numbers, and compare them as numbers if they are, otherwise return false
            if is_number(value1) && is_number(value2) {
                (value1.parse::<f64>().unwrap() < value2.parse::<f64>().unwrap()).to_string()
            } else {
                false.to_string()
            }
        }
        ">" => {
            // Check if the values are numbers, and compare them as numbers if they are, otherwise return false
            if is_number(value1) && is_number(value2) {
                (value1.parse::<f64>().unwrap() > value2.parse::<f64>().unwrap()).to_string()
            } else {
                false.to_string()
            }
        }
        _ => panic!("Unknown operator: {}", operator),
    }
}

// This function checks if a string can be parsed as a number
pub fn is_number(s: &str) -> bool {
    s.parse::<f64>().is_ok()
}
