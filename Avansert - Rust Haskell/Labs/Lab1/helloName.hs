
main :: IO ()
main = do
    putStrLn "Hello what you name?"
    a <- getLine
    print("Hello "++ a)

