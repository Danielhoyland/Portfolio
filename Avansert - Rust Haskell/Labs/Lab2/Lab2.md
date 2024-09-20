
# Lab 2

## Step 2.1
- Filename: step21

I remember i used a lot of time to begin on this but its the first time using Length, where and list comprehension which was very usefull for working further with haskell.

## Variant 2.1.1
- Filename: variant211

This is almost the same as step21 so didn't learn very much from it but just tok some time.

## Step 2.2
- Filename: step22

I mean this is usefull for a lot of situation to convert string to int. The only thing i would change now is to make it to maybe values so the program dont panic.

## Step 2.3
- filename: step23

This is step22 just with input from user. 

## Step 2.4
- Filename: step24

I had much troubles with this one. I managed to do it in the end and the layout of teh code of RanodmNumb was a good lesson for what is coming in the haskell department. Can't say i really learned the syntax of $ and map from this.

## Step 2.5
- Filename: step25

Same code as step24 but i didn't know at the time excatly what pipes was but was on the right track. Now ofc i know what it means and its very usefull.

## Step 2.6
- Filename: No file

Didn't do it. Didn't have time, didn't understand and didn't know how to do step25 so i didn't do this.

## Formatted Multiplication table
- filename: formattedTableButBetter

I had a really hard time with figuring out how to convert hexdeciaml to make the nice looking table with your Unicode characters, but was really happy about how it looked at the end. I did get som comments for improvements about the modularity of some of the printouts and functions. This is not the fixed version but i know what i did wrong and what i would change if i did it again.

## Highlight
- The thing i am most proud of for Lab2 is would probably be formattedTableButBetter. Even if it was difficult to find out about the hexadecimal conversion, it was actually really fun to toy with, and was at teh time really proud of what i made here:
### Code:
```
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
```
