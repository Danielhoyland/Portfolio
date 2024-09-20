import Data.List
mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]

convertStringToList :: String -> [Int]
convertStringToList st = map read (words st)

main = do
    putStrLn "Give numbers like this: 1 2 3 4 5 6"
    input <- getLine
    print (mostFrequent (convertStringToList input))


