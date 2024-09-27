import Text.Read
import Data.List
import Data.Maybe 

checker :: String -> Int
checker a 
  | isJust(readMaybe a :: Maybe Int) = 1
  | isJust(readMaybe a :: Maybe Float) = 2
  | isJust(readMaybe a :: Maybe String) = 3
  | otherwise = 4

printStuff :: Int -> String -> String
printStuff a b
    | a==1 = "Integer, "++b
    | a==2 = "Float, "++b
    | a==3 = "You have entered string: "++read b
    | a==4 && b=="Mariusz" = "Lecturer's name"
    | a==4 = "Unrecognized input"
    | otherwise = "uh"


main = do
    input <- getLine
    let a = printStuff(checker input) input
    print a
    if input/="PLEASELETMEOUT"
        then main
    else print "You are freed from your prison"