
# Lab 1

## FizzBuzz
- Filenames:
    1. fizzBuzz
    2. fizz30

Learned some things about list like the .. inside it and a little about the logic.

## Hello "name"
- Filename: helloName

Learned how to get user input. Would have used what i learned here, if SDL2 for windows specifically actually allowed me to. Would have asked the user for how big of a board they wanted in the assignment 1.

## Read numbers
- filename: readNum

began to give me a clue over the type defining.

## Multiplication table
- Filenames:
    1. table
    2. formattedTable

reason for 2 files here is since i tried to make the multiplication table a little prettier in formattedTable

## Highlight
- The thing i am most proud of for Lab1 is would probably be formattedTable. Its recursion where i try to display something actually decent when coming to this task early in this course:
### Code:
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
    let a = 5
    let n = 0 :: Int 
    printNum a n
```
