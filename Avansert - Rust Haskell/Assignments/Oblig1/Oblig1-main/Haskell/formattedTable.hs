main :: IO ()



printNum x n | n==x*x = return()
printNum x n =
    do 
        putStr"  "
        let y = (n `mod` x)+1
        if n `mod` x==4
            then print(y* ((n`div`x)+1))
        else
            putStr(show(y* ((n`div`x)+1)))  
        printNum  x (n+1)


main = do
    let a = 5
    let n = 0 :: Int 
    printNum a n