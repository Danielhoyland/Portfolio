## Requirement sepcification
**User specification**
* Visualisation of board
* Placeing of stones
* Two players/two type of pieces(stones)
* Captureing of stones
* Allowing player to skip turn
* Detecting illagal moves
* Reading SGF files to automatically setup board states
**Dev specification**
* Gather info about groups like the liberty of the group
* Allowing for restarting of game
* Allowing to choose size og board
* Points counter
* Something to tell whos turn it is
* Not allowed to put stones outside of board
* Resizing the screen

## Implemented features
* Visualisation of board
* Placeing of stones
* Two players/two type of pieces(stones)
* Allowing player to skip turn (press S to skip)
* Gather info about groups like the liberty of the group (implemented but in complicated way)
* Something to tell whos turn it is (under board)
* Not allowed to put stones outside of board
* Resizing the screen(almost works)
## Not implemented features
* Captureing of stones (almost implemented, logic behind it doesnt work fully)
* Detecting illagal moves (detects some illagal moves but not all, logic behind it doesnt work fully)
* Reading SGF files to automatically setup board states
* Allowing for restarting of game (function implemented no button)
* Allowing to choose size of board (everything is setup for resize of board but dont know how to get the input from user, didnt work with the normal haskell input)
* Points counter 

## Non functional requirements
**code quality**
The readebility of the code i relatively good except the part where the logic begins. I it takes a complex datastructure to get the location and the liberty of each group for storage additonally all of the other functions to help with this adding to group where a lot hours have been put into and then poorly named diffrent variabels. The structure could also be better by moving the logic to its own haskell file without having it in the same file. 
**maintainability assessment**
It may be a little difficult to add/change stuff in the program for the future but most of it should be simple to modify. The biggest exception is the game logic where the way i did it made it very complex in terms of modifying it.
**documentation quality**
The code is commented and the variabels and functions have somewhat apropriate names but not 100% and people will have some struggle to read the code with the comments and such especially around the logic.
## Assesment specification 
Many of the specifiacation can be tested by lanching the program and see what there. The background data is a little more difficult to test. The way i tested data is by using the drawtext function i have to see the data on screen. That was the main way for me to test the grouping.
## Self assesment 
**Functional Assesment**
Many things in the program works like it should and many things are not yet implemented. I think everything for the simple stuff works like stone movement, board creation and skip works perfectly. The thing that works that i should probably change but are in too deep at this point is how i do grouping. I do grouping when you release a stone. That stone would then be added to a group and all info of that needs to be added to a new group or an exsisting group and so on. It became too complex very fast and i have used too much time just on thinking how to add a function for the data. Not developer friendly at all. The function i am talking about is addToGroup. 
**Reasoning for non-implementation**
I havent added any read from SGF files since i didn't have time. Score haven't been added yet since i don't know how to add the area score thingy but there is a score data. Addionally i have restart game function and endofgame function that arent implemented but not added to be connected to the program. Wanted to add it when i was finished with the main stuff but didn't have the time.
