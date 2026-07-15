import Data.List

mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]


main = do
    let list = [1,2,3,4,5,1,2,1,2]
    print (mostFrequent list)

--The O(n) estimate of my code would be O(n log n). This i s because of the sort. That makes it 
--take a lot more time the longer the list in an exponential way.
--A better way to do this algorithm is to use hash map or something. It would make it closer
-- to O(n) instead and is much mor linear in time usage. 
