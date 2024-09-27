### Learn you Haskell up to chapter 9 inclusive

- Lists, list comprehensions, ranges (Ch 1 and 2)
    1. I would say that I have a good understanding of lists and ranges. May no remember all of the diffrent commands to use on the lists and gets more difficult to comprehend when dealing with mixed types in a list or list of lists.
    2. I would say I have a somewhat good understanding of how list comprehensions work but have a more diffucult time of when and where to use them when i am coding myself.
    3. Example from something i did pFor, moslty around the :
```
import Data.List

mostFrequent :: [Int] -> Int
mostFrequent xs = minimum [n | n <- sort xs, length [x | x <- xs, x == n] == maxFreq]
    where maxFreq = maximum [length [x | x <- xs, x == n] | n <- xs]


main = do
    let list = [1,2,3,4,5,1,2,1,2]
    print (mostFrequent list)
```
- Types and Typeclasses (Ch 3)
    1. I have a good understanding in theory about the diffrent types and their diffrences in use etc. and their commands like show and read and so on, but when etually programing with it i will often get stuck on something little about something around types. 
    2. Example that i have done:
```
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
```

- Syntax in functions, guards, if, case of, where, let in (Ch 4)
    1. I have good knowlege of how the syntax work in haskell in function but recently found it more difficult to understand them while doing work in SDL2.
    2. I would say i have good understanding about guards, if and where
    3. I haven't really used case of and let in but thnik i understand how to use them in theory.
    3. Example from some of the above that i have done:
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
- Recursion and recursive type definitions and functions (Ch 5)
    1. I think i have a good understanding of recursion and have used it but may have some diffcuty to understand some parts of it.
    2. Some of my code around this: 
```
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
    putStrLn "Give a number"
    input <- getLine
    let a = (read input :: Int)
    let n = 0 :: Int 
    printNum a n
```
- Advanced functions, higher-order functions, curried functions, maps and filters, lambdas, function composition and function application, folds (Ch 6)
    1. I have little understanding about what curried functions is, don't even know if i have used it or not. 
    2. I have some understanding what fold and lambda is but would have problems implementing it into my code without any external help or something that says i have to use it in the code.
    3. I have a pretty good understanding i would say about filter and maps. Maps may be tricky sometime.
    4. Some problems with the function composition and function application but nothing too bad.
    5. Example code:
```
drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> World -> (MouseButton -> Bool) -> IO ()
drawWorld r (t, ti) (mx, my) w m = do
  renderPane (topLeft     $ panes w) TopLeft
  renderPane (topRight    $ panes w) TopRight
  renderPane (bottomLeft  $ panes w) BottomLeft
  renderPane (bottomRight $ panes w) BottomRight
  drawText r (pack $ show mx ++ ", " ++ show my) (100,100)
    where
      tw :: Double
      tw = fromIntegral $ SDL.textureWidth ti
      th = fromIntegral $ SDL.textureHeight ti
      -- s is the size of a rectangle for a single pane
      s = C.mkRect 0 0 (tw) (th)
      mFor c = s `C.rectMoveTo` (0,0)
      pFor c = if click (C.rectX (fmap round s)) (C.rectY (fmap round s)) 1000 1000 mx my  -- replaced (round tw) (round th) with 1000 while it doesnt save the data about the image
                then if (m SDL.ButtonLeft) 
                      then s `C.rectMoveTo` (fromIntegral mx, fromIntegral my)
                      else s `C.rectMoveTo` (C.rectXY (fmap round s))
                else s `C.rectMoveTo` (C.rectXY (fmap round s))
     
      renderPane p q
        = SDL.copy r t
          (Just $ floor <$> mFor p)
          (Just $ floor <$> pFor q)
```
- Modules and some standard modules (Ch 7)
    1. I understand how to import the many of the modules and can use them relatively good but have to look up the name of the function but kinda knows what modlue i need things from.
    2. I have used some of the functions from the diffrent modules but dont feel like i use them very much. (I use them more and more when dealing with the assignment)
    3. Example when i used the Data.char module:
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
- Types and typeclasses, declaring own, deriving (Ch 8)
    1. I have not been learning a lot about these subject but am understanding more and more gow to use type, data, deriving since i need to understand code examples for SDL for the assignment
    2. No own code example, borrowed code that i kinda understand:
```
data Intent
  = Idle
  | Quit
  | Move Coord
  | Press Coord
  | Release Coord

type Coord = (CInt, CInt)

data Circle = Circle
  { pos :: Coord
  , radius :: CInt
  , sticky :: Bool
  , captured :: Coord
  } deriving (Show)

data World = World
  { exiting :: Bool
  , circles :: [Circle]
  }
```
- I/O, streams, bytestream, Text vs Straing, buffers, randomness, I/O exception handling (Ch 9)
    1. I would say i have relatively good understanding about these subjects but haven't really used a lot of it so i don't quite want to try to comment on that, haha. 
    2. I have mostly used print, getline, randomness.
    3. I have an basic understanding on the rest i think but need to look it kinda up if i need to use some of the other stuff.
    4. Some code:
```
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
```

