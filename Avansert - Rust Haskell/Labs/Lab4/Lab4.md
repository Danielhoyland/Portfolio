# Lab 4

## GoGame
- Filename: GoGame

This is was a good begining for the Assignment1. The data and types was notiations from a thursday lecture. I really should have noted the usefullnesss of type and data but didn't. Can't say much of this was used for the Assignment but some was. It was like an whole other programming "style" if you can call it that when we began to program with SDL2. 

The few functions i took insperation from or kinda used fully was place, changeValue and board. I used board basically fully while place and changeValue wasn't directly used but i used the consept of them in the Assignment where i split up values and sent it to another function again for mudlarity, clearity and non-spaggeti.

## Highlight
- Its really hard to say what function i am most proud of here but i think it has to be changeValue and place togheter since they interact with eachother and both was usefull as concepts for myself later on:
### Code:
```
--changes the value into the 2d list
changeValue :: Int -> a -> [a] -> [a]
changeValue i x b = take i b ++ [x] ++ drop (i+1) b

--placeing stones into the list
place :: Int -> Int -> [[Int]]-> Int -> [[Int]]
place x y b p = 
         changeValue x (changeValue y p (b!!x)) b
```