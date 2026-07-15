// Calculates the result of applying the given operator to the two given numbers
// Returns the calculated result as a float
// Throws a panic if the operator is not recognized
pub fn calculate(operator: &str, x: f64, y: f64) -> f64 {
    match operator {
        "+" => x + y,
        "-" => x - y,
        "*" => x * y,
        "/" => format!("{:.1}", x / y).parse().unwrap(),
        "div" => (x / y).floor(),
        "mod" => x % y,
        _ => panic!("Unknown operator: {}", operator),
    }
}

// Determines whether the given operator is a recognized math operator
// Returns true if the operator is recognized, false otherwise
pub fn check_if_math(operator: &str) -> bool {
    match operator {
        "+" => true,
        "-" => true,
        "*" => true,
        "/" => true,
        "div" => true,
        "mod" => true,
        _ => false,
    }
}
