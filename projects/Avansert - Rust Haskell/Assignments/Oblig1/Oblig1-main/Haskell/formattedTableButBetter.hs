import Data.Char
import Numeric (readHex)

uni x 
    | x == 0 = head [dec |let [(dec, _)] = readHex "2500"] -- ─
    | x == 1 = head [dec |let [(dec, _)] = readHex "2502"] -- │
    | x == 2 = head [dec |let [(dec, _)] = readHex "253C"] -- ┼
    | x == 3 = head [dec |let [(dec, _)] = readHex "250C"] -- ┌
    | x == 4 = head [dec |let [(dec, _)] = readHex "2510"] -- ┐
    | x == 5 = head [dec |let [(dec, _)] = readHex "2524"] -- ┤
    | x == 6 = head [dec |let [(dec, _)] = readHex "251C"] -- ├
    | x == 7 = head [dec |let [(dec, _)] = readHex "2534"] -- ┴
    | x == 8 = head [dec |let [(dec, _)] = readHex "252C"] -- ┬
    | x == 9 = head [dec |let [(dec, _)] = readHex "2514"] -- └
    | x == 10 = head [dec |let [(dec, _)] = readHex "2518"] -- ┘
    | otherwise = head [dec |let [(dec, _)] = readHex "0"]

printNum x n | n==x*x = putStrLn [chr (uni 9), chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 7),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 7),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 7),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 7),chr (uni 0), chr (uni 0), chr (uni 0),chr(uni 10)]>>return()
printNum x n =
    do 
        let y = (n `mod` x)+1
        let p = y * ((n `div` x) + 1)
        
        if (n==4 || n==9 || n==14 || n==19) && p<10
            then putStr (" "++show p++" ") >> putStrLn [chr (uni 1)] >>
            putStrLn [chr (uni 6), chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0),chr(uni 5)]>>putStr [chr (uni 1)]
        else if (n==4 || n==9 || n==14 || n==19)
            then putStr (" "++show p) >> putStrLn [chr (uni 1)] >>
            putStrLn [chr (uni 6), chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 2),chr (uni 0), chr (uni 0), chr (uni 0),chr(uni 5)]>>putStr [chr (uni 1)]
        else if n==24
            then putStr (" "++show p)>>putStrLn [chr (uni 1)]
        else if p<10
            then putStr (" "++show p++" ")>>putStr [chr (uni 1)]    
        else 
            putStr (" "++show p)>>putStr [chr (uni 1)]
        printNum  x (n+1)






main = do 
    putStrLn [chr (uni 3), chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 8),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 8),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 8),chr (uni 0), chr (uni 0), chr (uni 0), chr (uni 8),chr (uni 0), chr (uni 0), chr (uni 0),chr(uni 4)]
    putStr [chr (uni 1)]
    let a = 5
    let n = 0 :: Int 
    printNum a n
