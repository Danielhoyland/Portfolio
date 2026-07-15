import Data.List
import Data.Char


data Stone = Black | White

type Coord = (Int,Int)

type Board = [[Maybe Stone]]

--creating the 2d board list
board :: Int->[[Int]]
board x = replicate x (replicate x 0)       


--prints out the board
boardPlace :: (Show a1, Show a2) => Int -> Int -> Int -> [[a2]] -> [a1] -> IO ()
boardPlace x (-2) z b a = return()
boardPlace x y z b a = 
    do  
        if y==(-1) && x==z
            then  putStrLn(" "++show (a !! x)) >> 
            boardPlace (x-z) (y-1) z b a
        else if y==(-1)
            then  putStr(" "++show (a !! x)) >> 
            boardPlace (x+1) y z b a
        else if x==z
            then putStrLn(" "++show ((b !! y) !! x)) >>
                boardPlace (x-z) (y-1) z b a
        else 
            putStr(" "++show ((b !! y) !! x)) >>
            boardPlace (x+1) y z b a
    

--player white or black placeses a stone on the board
pickPlace :: Int -> [[Int]] -> Int -> IO b
pickPlace x b p = do
    let o = if p == 1 then p + 1 else p - 1
    let alphabets=[chr x | x<-[65..(65+x)]]
    boardPlace 0 (x-1) (x-1) b alphabets
    if p==1
        then print"Blacks turn, what do you want to do?"
    else
        print"Whites turn, what do you want to do?" 
    print"type a a cordinate like 4E, where the number is the y value, BIG LETTER" 
    d <- getLine
    let (dd,kk) = splitAt 1 d
    let v = (ord (head kk)-64)::Int
    let w = read dd::Int
    
    let q = place (w-1) (v-1) b p
    pickPlace x q o

   
--changes the value into the 2d list
changeValue :: Int -> a -> [a] -> [a]
changeValue i x b = take i b ++ [x] ++ drop (i+1) b

--placeing stones into the list
place :: Int -> Int -> [[Int]]-> Int -> [[Int]]
place x y b p = 
         changeValue x (changeValue y p (b!!x)) b
    

--main, call board showing and call to get player to place stones
main = do
    --print "What do you want to do?"
    --print "1: Show current Board"
    print"How big do you want the board? Squared" 
    input <- getLine
    let x = read input::Int
    let b = board x
    let alphabets=[chr x | x<-[65..(65+x)]]
    boardPlace 0 (x-1) (x-1) b alphabets
    pickPlace x b 1 

