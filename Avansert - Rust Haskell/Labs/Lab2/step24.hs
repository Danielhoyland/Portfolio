import Data.List
import System.Random

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
    print (convertStringToList(randomNumb x y))

