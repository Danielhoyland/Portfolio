# Lab 6

## The movement of stone pictures
- Filename(s): the whole dir
- Use cabal build and then cabal run GoGame-exe to try to run this

In the file dir in src you see Common, Lib and Ver1 files. Ver1 was my try to get it to work I kinda worked but in the end I instead took and used the shared file for movement of stones. If you want to try to run my program for this lab use the link to the lab6 on gitlab for my mergerequest there since the Common file i used for Ver1 was edited and removed some functions i used for it. 

### Improvements and what i learned
- This was the first real SDL2 program i tried to program. It was very difficult to try to understand what was happening and a lot of the code was jsut a mess since i just took SDL2mouse program for the shared file from gitlab.

- Now i know much more and would instead try to understand what happened, but I think it was a very good try from my side. I began to understand how to use SDL2 and was really proud of manging of getting postion and moving of the stone. The only thing i didn't manage to do this alone was the when i releasd the stone it went back to earlier postion and didn't manage to fix this issue. 

## Highlight
- Most of the time working on this lab I used to try to understand and practically just trying stuff. But in the end I didn't use any of this but the thing I am most proud of is the two function below to get postion of the mouse and if clicking is on stone or not:
### Code:
```
getPositionXY :: IO (Int, Int)
getPositionXY = do
  P (V2 x y) <- getAbsoluteMouseLocation
  return (fromIntegral x, fromIntegral y)

click :: Int-> Int -> Int -> Int -> Int -> Int -> Bool
click x y tw th mx my
  | x <= mx && mx <= (x + tw) && y <= my && my <= (y + th) = True
  | otherwise = False
```