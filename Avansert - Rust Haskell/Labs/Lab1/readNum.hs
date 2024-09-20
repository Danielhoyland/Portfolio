main :: IO ()
main = do
    putStrLn "Give a number"
    input <- getLine
    let a = (read input :: Float)
    putStrLn "Give another number"
    input <- getLine
    let b = (read input::Float)
    print(a*b)
