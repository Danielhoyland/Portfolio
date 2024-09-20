# Lab 3

## Task 3.1
- Filename: step31

I was really confused by this task and still am. Can't say i learned anything from this.
 
## Task 3.2 and Task 3.3
- Filename: Step323

The begining of the SDL2 pain. Was not able to do there two task since i didn't manage to download SDL2

## Task.3.4
- Filename: step34

This was a very usefull and i learned a lot from this without knowing. I just really hoped i remember this task when doing assignment 2. 

## Highlight
- The thing i am most proud of for Lab3 is would probably be step34. I mean its the only task i actually coded for this but i am proud of it anyways. Got to learn about maybe and strengthend my understanding of some of the code aspect:
### Code:
```
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
```
