import Data.List
import System.Random

mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]

convertStringToList :: String -> [Int]
convertStringToList st = map read (words st)

randomNumb :: Int -> Int -> String
randomNumb n maxV = unwords $ map show $ take n $ randomRs (0,maxV) (mkStdGen 1)

main = do
    putStrLn "How many numbers do you want?"
    n <- getLine
    putStrLn "Whats the max value of then numbers?"
    maxV <- getLine
    let x = read n :: Int
    let y = read maxV :: Int
    print (randomNumb x y)
    --Dont know what pipes are. I would guess its the | something |something else thingy but dont know how to implement it related to the step



