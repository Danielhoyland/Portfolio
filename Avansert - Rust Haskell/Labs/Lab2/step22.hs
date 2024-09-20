import Data.List
mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]

convertStringToList :: String -> [Int]
convertStringToList st = map read (words st)

main = do
    let strNumb = "1 4 5 2 4 3 1 2 1"
    print (mostFrequent (convertStringToList strNumb))

