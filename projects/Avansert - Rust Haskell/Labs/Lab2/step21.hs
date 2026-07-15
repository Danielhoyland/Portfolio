import Data.List

mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]


main = do
    let list = [1,2,3,4,5,1,2,1,2]
    print (mostFrequent list)

