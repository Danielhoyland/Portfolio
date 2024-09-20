import Data.List

mostFrequent :: [Int] -> [Int]
mostFrequent xs = [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]


main = do
    putStrLn "Give a list with numbers like this: [1,2,3]"
    input <- getLine
    let list = read input
    print (mostFrequent list)

