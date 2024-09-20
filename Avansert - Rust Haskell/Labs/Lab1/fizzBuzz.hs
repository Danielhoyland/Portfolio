
main = do
    print (fizzBuzz 15)

fizzBuzz x
  | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x