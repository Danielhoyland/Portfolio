module Lib
    ( someFunc
    , mainApp
    ) where


import qualified SDL
import qualified SDL.Font
import qualified Common  as C
import           Data.Foldable  (foldl')
import           Control.Monad  (when)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text (pack)
import qualified Data.List as Lcom


-- | Dummy function, returns "Hello " concatenated with a passed argument.
-- 
--
-- >>> someFunc "Dave"
-- "Hello Dave"
someFunc :: String -> String
someFunc name = "Hello " ++ name

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

type Position = (Int, Int)
data Color = Black | White deriving (Show, Eq)
type StoneGroup = (Color, [Position])
type ListPosition = (Int, Int)

-- Intents
data Intent
  = Idle
  | Quit
  | Skip
  | Reset
  | WindowResized (SDL.V2 Int)
  | MouseMoved (Int, Int)
  | MouseButtonReleased
  | MouseButtonPressed
  deriving (Show)

-- The world
data World = World { exiting                :: Bool
                   , windowDimensions       :: (Int, Int)
                   , boardSize              :: Int
                   , xyOffset               :: Int
                   , squareSize             :: Int
                   , worldDragging          :: Bool
                   , worldMousePos          :: Position
                   , worldTextureBackground :: (SDL.Texture, SDL.TextureInfo)
                   , worldTextureBlack      :: (SDL.Texture, SDL.TextureInfo)
                   , worldTextureWhite      :: (SDL.Texture, SDL.TextureInfo)
                   , worldWhiteStoneXY      :: Position
                   , worldBlackStoneXY      :: Position
                   , worldDraggedColor      :: Color
                   , worldStones            :: [StoneGroup]
                   , board                  :: [[Maybe Color]]
                   , turn                   :: Color
                   , textPos                :: Position
                   , whoMove                :: Position
                   , skipNum                :: Int
                   , score                  :: [(Color, Int)]
                   , group                  :: [(Color, [(Int, ([ListPosition],[ListPosition]))])]
                   }


worldTexture :: Color -> World -> SDL.Texture
worldTexture White = fst . worldTextureWhite
worldTexture Black = fst . worldTextureBlack



flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White



initialWorld :: Int -> World
initialWorld x = World 
  { exiting                 = False
  , windowDimensions        = (0, 0)
  , boardSize               = x -- default game 9x9
  , xyOffset                = 1 -- dummy offset and square size
  , squareSize              = 1
  , worldStones             = [(White, []),(Black, [])]
  , worldDragging           = False
  , worldMousePos           = (0,0)
  , worldTextureBackground  = undefined
  , worldTextureBlack       = undefined
  , worldTextureWhite       = undefined
  , worldWhiteStoneXY       = undefined
  , worldBlackStoneXY       = undefined
  , worldDraggedColor       = undefined
  , board                   = replicate x (replicate x Nothing)
  , turn                    = Black
  , textPos                 = undefined
  , whoMove                 = undefined
  , skipNum                 = 0
  , score                   = [(White, 0),(Black, 0)]
  , group                   = [(White, []),(Black, [])]
  }


-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp ::  Int -> SDL.Window -> IO ()
mainApp x w =
    C.withRenderer w $ \r -> do
        -- lets load the background texture
        SDL.Font.initialize
        textureBackground <- C.loadTextureWithInfo r "./assets/wood.png"
        textureWhite <- C.loadTextureWithInfo r "./assets/stone_white.png"
        textureBlack <- C.loadTextureWithInfo r "./assets/stone_black.png"
        -- obtain the size of the window
        (SDL.V2 winWidth winHeight) <- fmap fromIntegral <$> (SDL.getWindowSurface w >>= SDL.surfaceDimensions)
        
        -- Initial world with the Window dimensions
        let initWorldWithoutTextures =
                    resizeWorldWindow (winWidth, winHeight) (Lib.initialWorld x)
        let initWorld = initWorldWithoutTextures {
              worldTextureBackground = textureBackground
            , worldTextureWhite = textureWhite
            , worldTextureBlack = textureBlack
        }


        -- we create an utility curry for us here
        let doRender = Lib.renderWorld r

        let loop world = do
                events <- SDL.pollEvents
                let newWorld = updateWorld world events
                doRender newWorld
                if Lib.exiting newWorld then return () else loop newWorld

        loop initWorld

        -- when we are done with renderering, we need to clean up
        SDL.Font.quit
        mapM_ (SDL.destroyTexture . fst) [ worldTextureBackground initWorld
                                         , worldTextureWhite initWorld
                                         , worldTextureBlack initWorld ]



-- =======================================================================
-- Events
-- =======================================================================

-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
    = foldl' (flip applyIntent) w
    . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit 
  else if SDL.keyboardEventKeyMotion e == SDL.Released && SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeS then Skip
  else if SDL.keyboardEventKeyMotion e == SDL.Released && SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeR then Reset else Idle
payloadToIntent (SDL.WindowResizedEvent e) = WindowResized $ C.mkV2Int size
  where
    size = SDL.windowResizedEventSize e
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle


-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e= MouseMoved (fromIntegral x, fromIntegral y)
  where
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = if SDL.mouseButtonEventMotion e == SDL.Pressed
                    then MouseButtonPressed
                    else MouseButtonReleased


applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld
applyIntent Skip        = skipMove
applyIntent Reset       = resetGame
applyIntent (WindowResized (SDL.V2 width height)) = resizeWorldWindow (width, height)
applyIntent MouseButtonPressed  = startDragging
applyIntent MouseButtonReleased = stopDragging
applyIntent (MouseMoved pos) = mouseMovedToPosition pos


-- =======================================================================
-- Event handlers
-- =======================================================================

-- do nothing
idleWorld :: World -> World
idleWorld = id

-- let's exit
quitWorld :: World -> World
quitWorld w = w { exiting = True }
--if someone skips it will change whos turn it is and add one to the skip counter to use later for end of game
skipMove :: World -> World
skipMove w = w { turn = flipColor (turn w)
                ,skipNum = (skipNum w) + 1}
--resets the game values to start a new game
resetGame :: World -> World
resetGame w = w {   exiting                 = False
                  , worldStones             = [(White, []),(Black, [])]
                  , worldDragging           = False
                  , board                   = replicate (boardSize w) (replicate (boardSize w) Nothing)
                  , turn                    = Black
                  , skipNum                 = 0
                  , score                   = [(White, 0),(Black, 0)]
                  , group                   = [(White, []),(Black, [])]
  }

--starts the dragging prosess of moving the stone
startDragging :: World -> World
startDragging w = if stonePicked then w { worldDragging = True, worldDraggedColor = colorPicked } else w
  where
    selectedColor   = checkUserColorSelection
    stonePicked     = isJust selectedColor
    colorPicked     = fromJust selectedColor
    checkUserColorSelection
        | (turn w) == White && mx+q >= wx && my+q >= wy && mx+q <= wx + size && my+q <= wy + size = Just White
        | (turn w) == Black && mx+q >= bx && my+q >= by && mx+q <= bx + size && my+q <= by + size = Just Black
        | otherwise = Nothing
        where
          size = squareSize w
          (mx, my) = worldMousePos w
          (wx, wy) = worldWhiteStoneXY w
          (bx, by) = worldBlackStoneXY w
          q = stoneRad w

--stop dragging the stone then checks if its legal, adds it to group fixed board state, and capture stone
--groups if they are going to be taken
stopDragging :: World -> World
stopDragging w
    | worldDragging w = w {
           worldDragging = False
         , worldStones = if check && (snd groupPlace) then if (fst takes) then fixedWordStones else updatedStones
                         else worldStones w
         , board = if check && (snd groupPlace) then if (fst takes) then fixedBoard else place listPlaceY listPlaceX (board w) (Just draggedColor)
                   else board w
         , turn = if check && (snd groupPlace) then flipColor draggedColor
                  else turn w
         , skipNum = if check && (snd groupPlace) then 0
                  else skipNum w
         , group = if check && (snd groupPlace) then if (fst takes) then fixedGroup else fst(groupPlace)
                   else group w}
    | otherwise = w
  where
    draggedColor            = worldDraggedColor w
    positions               = fromJust $ lookup draggedColor (worldStones w)
    theOtherGroupPositions  = fromJust $ lookup (flipColor draggedColor) (worldStones w)
    updatedStones           = (flipColor draggedColor, theOtherGroupPositions):[(draggedColor, placement)]
    mouseXY                 = worldMousePos w
    placement               = (xPosStone, yPosStone):positions
    xPosStone               = fst (fst boardPlace)
    yPosStone               = fst (snd boardPlace)
    listPlaceX              = snd (fst boardPlace)
    listPlaceY              = snd (snd boardPlace)
    boardPlace              = boardPos w (mouseXY)
    check                   = (checkAvailable listPlaceY listPlaceX (board w))
    groupPlace              = addToGroup w (listPlaceY,listPlaceX) draggedColor
    takes                   = if draggedColor==White then takeStone (snd(last(fst(groupPlace)))) (length(snd(last(fst(groupPlace))))) w draggedColor
                              else takeStone (snd(head(fst(groupPlace)))) (length(snd(head(fst(groupPlace))))) w draggedColor
    fixedGroup              = if draggedColor==White then [head(fst(groupPlace)),((fst(last((fst(groupPlace))))),(snd(last(fst(groupPlace)))) Lcom.\\ [(snd takes)])]
                              else [((fst(head(fst(groupPlace)))),(snd(head(fst(groupPlace)))) Lcom.\\ [(snd takes)]),last(fst(groupPlace))]
    fixedWordStones         = [(fst(head(updatedStones)),filter (\t -> t /= (-1, -1)) worldStoneList),(last(updatedStones))]
    worldStoneList          = [if Lcom.find(==((snd(snd(boardPos w ((snd(head(updatedStones)))!!(x-1))))),(snd(fst(boardPos w ((snd(head(updatedStones)))!!(x-1))))))) (fst(snd(snd takes)))==Nothing then ((snd(head(updatedStones)))!!(x-1)) else (-1,-1)|x<-[1..(length(snd(head(updatedStones))))]]
    fixedBoard              = if allCoordsMatch (place listPlaceY listPlaceX (board w) (Just draggedColor)) draggedColor (fst(snd(snd takes))) then setCoordsToNothing (place listPlaceY listPlaceX (board w) (Just draggedColor)) draggedColor (fst(snd(snd takes))) else (place listPlaceY listPlaceX (board w) (Just draggedColor))

-- Check if all Maybe colors at the given coordinates match the flipped color
allCoordsMatch :: [[Maybe Color]] -> Color -> [ListPosition] -> Bool
allCoordsMatch xs c cs = all (\(i,j) -> maybe False (== flipColor c) (xs !! i !! j)) cs

-- Define a function to set all Maybe colors at the given coordinates to Nothing if they match the flipped color
setCoordsToNothing :: [[Maybe Color]] -> Color -> [ListPosition] -> [[Maybe Color]]
setCoordsToNothing xs c cs = [[if (i,j) `elem` cs && maybe False (== flipColor c) (xs !! i !! j) then Nothing else xs !! i !! j | j <- [0..length xs - 1]] | i <- [0..length xs - 1]]


mouseMovedToPosition :: (Int, Int) -> World -> World
mouseMovedToPosition (x,y) w = w { worldMousePos = (x-q,y-q)}
  where
    q = stoneRad w

-- window has been resized
resizeWorldWindow :: (Int, Int) -> World -> World
resizeWorldWindow (width, height) w = w
                               { windowDimensions = (width, height)
                               , xyOffset = round xOffset
                               , squareSize = round rectSizeF
                               , worldWhiteStoneXY = whiteStonePrototypeXY
                               , worldBlackStoneXY = blackStonePrototypeXY
                               , textPos = textPrototypeXY
                               , whoMove = whoMovePrototypeXY
                               , worldStones = stoneList
                               }
  where
    gameDim = boardSize w
    maxRectSizeF = min ((fromIntegral width::Double) * (0.8::Double) /
                       (fromIntegral gameDim::Double)) (fromIntegral height * 0.7 / fromIntegral gameDim)
    rectSizeF = min maxRectSizeF (min (fromIntegral width) (fromIntegral height) / fromIntegral gameDim)
    xOffset = min ((0.1::Double) * fromIntegral width) (0.1 * fromIntegral height)
    whiteStonePrototypeXY = (round (rectSizeF * (fromIntegral gameDim + 2)), round (2 * xOffset))
    blackStonePrototypeXY = (round (rectSizeF * (fromIntegral gameDim + 2)), round (3 * xOffset))
    textPrototypeXY = (round (rectSizeF * (fromIntegral gameDim + 2))-30, round (4 * xOffset))
    whoMovePrototypeXY = (round (xOffset), round (rectSizeF * (fromIntegral gameDim + 1)))
    stoneList = stoneResizeXY (round rectSizeF) (squareSize w) listLenght (worldStones w)
    listLenght = ((length (worldStones w)) - 1)


stoneResizeXY ::  Int->Int->Int -> [StoneGroup] -> [StoneGroup]
stoneResizeXY rectSize squareSize1 p xss = posStone
  where
    posStone  = [((fst(xss!!z)),[((((fst ((snd ((xss!!z)))!!(y)))*rectSize)`div` squareSize1), (((snd ((snd ((xss!!z)))!!(y))) * rectSize)`div` squareSize1))| y<-[0..(length(snd(xss!!z)))-1]])| z<-[0..p]]
-- =======================================================================
-- Rendering
-- =======================================================================

-- Given the renderer, and the state of the World,
-- we can render the world. Note that the rendering results is an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r world = do
    SDL.clear r
    drawBackground r backgroundTexture winDim
    drawEmptyGameBoard r gameBoardSize offset squareDim world
    drawText r (pack $ "Press S to skip turn") (textPos world) black
    drawText r (pack $ "Press R to reset game") ((fst (textPos world)), (snd (textPos world)) +20) black
    drawText r (pack $ (show (turn world)) ++ " to move") (whoMove world) black
    -- we need to draw the two stones that can be dragged
    SDL.copy r (fst . worldTextureBlack $ world) Nothing (Just $ C.mkRect bx by squareDim squareDim)
    SDL.copy r (fst . worldTextureWhite $ world) Nothing (Just $ C.mkRect wx wy squareDim squareDim)

    -- we need to draw all the already "dragged and dropped" stones
    mapM_ (\(color, positions) -> do
                    mapM_ (\(x, y) -> do
                            SDL.copy r (worldTexture color world)
                                Nothing (Just $ C.mkRect x y squareDim squareDim)
                      ) positions
                ) (worldStones world)

    -- we need to draw the dragged stone
    when (worldDragging world) $ SDL.copy r (worldTexture draggedColor world)
                                Nothing (Just $ C.mkRect mx my squareDim squareDim)

    SDL.present r
  where
    winDim        = windowDimensions  world
    gameBoardSize = boardSize         world
    squareDim     = squareSize        world
    offset        = xyOffset          world
    backgroundTexture = worldTextureBackground world
    (bx, by)      = worldBlackStoneXY world
    (wx, wy)      = worldWhiteStoneXY world
    (mx, my)      = worldMousePos     world
    draggedColor  = worldDraggedColor world

-- The actual method for drawing the background
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do
    let texHeight = SDL.textureHeight ti
        texWidth  = SDL.textureWidth ti

    -- Loop and draw the background texture, tiling it if necessary
    let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + fromIntegral texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ C.mkRect (fromIntegral x) (fromIntegral y)
                                                     texWidth texHeight)
              loop (x + fromIntegral texWidth) y

    loop 0 0


-- Draw the game board.
drawEmptyGameBoard :: SDL.Renderer -> Int -> Int -> Int -> World -> IO ()
drawEmptyGameBoard r _ offset square w = do
    -- color hardcoded here
    SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
    SDL.drawRect r . Just $ C.mkRect xy xy (wh*(p-1)) (wh*(p-1))
    drawLines r xy wh wh p p
  where
    xy      = offset - 1
    wh      = ((p*square)) `div` p
    p       = boardSize         w

-- the radius of the stone in a function for easy use
stoneRad :: World ->Int
stoneRad w = round ((fromIntegral (squareSize w)) / 2 :: Double)

--Takes x and y and gives back the square where it was placed and the both list numbers to change 2d list
boardPos ::  World -> (Int,Int) ->((Int,Int),(Int,Int))
boardPos w (x,y) = ((xPos x xy wh wh p 0),(yPos y xy wh wh p 0))
  where 
    xy      = offset - 1
    wh      = ((p*square) `div` p)
    offset  = xyOffset          w
    p       = boardSize         w
    square  = squareSize        w

--checks the what "x box" that the player wanted to place their stone in
-- takes x the square offset, the box length two times, boardsize and a which is 0 in the start
xPos :: Integral a => a -> a -> a -> a -> a -> a -> (a,a)
xPos x offset xy yx p a
  | xy>yx*p = (x,(-1))
  | x >= (yx `div` 2) && x < (yx `div` 2) + xy = (offset + xy - yx - (yx `div` 2), a)
  | otherwise = xPos x offset (xy + yx) yx p (a+1)

--checks the what "y box" that the player wanted to place their stone in
-- takes x the square offset, the box length two times, boardsize and a which is 0 in the start
yPos :: Integral a => a -> a -> a -> a -> a -> a -> (a,a)
yPos y offset xy yx p a
  | xy>yx*p = (y,(-1))
  | y >= (yx `div` 2) && y < (yx `div` 2) + xy = (offset + xy - yx - (yx `div` 2), a)
  | otherwise = yPos y offset (xy + yx) yx p (a+1)

--draws one line horsontal and one downwards inside of the board
drawLines::SDL.Renderer -> Int -> Int -> Int -> Int -> Int -> IO()
drawLines _ _ _ _ _ 2 = do return()
drawLines r xy wh yx p q = do 
  SDL.drawLine r (C.mkPoint (xy+wh) xy) (C.mkPoint (xy+wh) (xy+(yx*(p-1))))
  SDL.drawLine r (C.mkPoint xy (xy+wh)) (C.mkPoint (xy+(yx*(p-1))) (xy+wh))
  drawLines r xy (wh+yx) yx p (q-1)

--changes the value into the 2d list
changeValue :: Int -> a -> [a] -> [a]
changeValue i x b = take i b ++ [x] ++ drop (i+1) b

--placeing stones into the list
place :: Int -> Int -> [[Maybe Color]]-> Maybe Color -> [[Maybe Color]]
place x y b p = 
         changeValue x (changeValue y p (b!!x)) b

-- | Let us draw some text
drawText :: SDL.Renderer -> Text -> (Int, Int) -> SDL.Font.Color -> IO ()
drawText r t (x, y) tone = do
        font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 70
        textSurf <- SDL.Font.solid font tone t
        SDL.Font.free font
        surf <- SDL.createTextureFromSurface r textSurf
        info <- SDL.queryTexture surf
        let w = SDL.textureWidth info
        let h = SDL.textureHeight info
        SDL.copy r surf Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 (w `div` 5) (h `div` 5)))
        SDL.destroyTexture surf
        SDL.freeSurface textSurf

--checks if the moving of stone is inside the board
checkAvailable :: Int -> Int -> [[Maybe Color]] ->Bool
checkAvailable x y xss 
        | x==(-1) || y==(-1) = False
        | xss !! x !! y /= Nothing = False
        | otherwise = True

--end of game function where it returns true if a statement that would end the game is true
endOfGame :: World -> Bool
endOfGame w 
        | skipNum w >= 2 = True
        | otherwise = False

--add new stone to group if beside a group or makes it its own group. May also add another group to the group if there was two or more group beside the placed stone
addToGroup :: World->(Int, Int)->Color->([(Color, [(Int, ([ListPosition],[ListPosition]))])],Bool)
addToGroup w (a,b) c 
              |  ifBeside && (ifLegal lastList w c) = ((lastList),True)
              |  not (ifBeside) && (ifLegal noBesideList w c) = ((noBesideList),True)
              |  otherwise = ((group w),False)
  where
    ifBeside = fst neighbour 
    noBesideList = if c==White && (length fGroup)/=0 then [(White, fGroup++[((fst(fGroup!!((length fGroup)-1)))+1, ([(a,b)],addLiberty ((boardSize w)-1) [(a,b)] (a,b) [] (4)))]),last(groupPoint)]
                   else if c==Black && (length fGroup)/=0 then [head(groupPoint),(Black, fGroup++[((fst(fGroup!!((length fGroup)-1)))+1, ([(a,b)],addLiberty ((boardSize w)-1) [(a,b)] (a,b) [] (4)))])]
                   else if c==White then [(White, fGroup++[(1, ([(a,b)],addLiberty ((boardSize w)-1) [(a,b)] (a,b) [] (4)))]),last(groupPoint)]
                   else [head(groupPoint),(Black, fGroup++[(1, ([(a,b)],addLiberty ((boardSize w)-1) [(a,b)] (a,b) [] (4)))])]
    neighbour  = (getOther w a b (Just c)) 
    groupPoint = (group w) 
    groupNumPlace = getGroupNum fGroup ((snd(neighbour))) ((length fGroup))
    fGroup = if fst(head(groupPoint)) == c then snd(head(groupPoint))
             else snd(last(groupPoint))
    groupNewLiberty = ((snd(snd(fGroup!!(snd groupNumPlace)))) Lcom.\\ [(a,b)]) -- seems to take the liberty of the group and remove (a,b)?
    combineGroup = ((fst(snd(fGroup!!(snd groupNumPlace))))++[(a,b)]++(fst groupToGroupCombine),((((groupNewLiberty++(addLiberty ((boardSize w)-1) (fst(snd(fGroup!!(snd groupNumPlace)))) (a,b) [] (4))++(snd groupToGroupCombine)) Lcom.\\ (fst groupToGroupCombine))Lcom.\\ [(a,b)]) Lcom.\\ (fst(snd(fGroup!!(snd groupNumPlace))))))
    groupsTheWantToCombine = checkGroupToGroup (fst(groupNumPlace)) (fGroup) (a,b) (length fGroup) [] 
    groupToGroupCombine = (addGroups (a,b) fGroup (groupsTheWantToCombine) (length groupsTheWantToCombine) [] [])
    lastList = if c==White then [(White, ([if (x-1)/=snd(groupNumPlace) then (snd(head(groupPoint)))!!(x-1) else ((x),(combineGroup)) | x<-[1..((length(snd(head(groupPoint)))))]] Lcom.\\ delLastList)),last(groupPoint)]
               else [head(groupPoint),(Black, ([if (x-1)/=snd(groupNumPlace) then (snd(last(groupPoint)))!!(x-1) else ((x),(combineGroup)) | x<-[1..((length(snd(last(groupPoint)))))]] Lcom.\\ delLastList))]
    delLastList = if c==White then [(snd(head(groupPoint)))!!(groupsTheWantToCombine!!(x-1)) | x<-[1..((length groupsTheWantToCombine))]]
               else [(snd(last(groupPoint)))!!(groupsTheWantToCombine!!(x-1)) | x<-[1..((length groupsTheWantToCombine))]]
    
--gets the liberty and position of another group that is combining with the "main" group
addGroups :: (Int,Int) -> [(Int, ([ListPosition],[ListPosition]))] -> [Int] -> Int -> [ListPosition] -> [ListPosition] -> ([ListPosition],[ListPosition])
addGroups (_,_) _ _ 0 list1 list2 = (list1,list2)
addGroups (a,b) lib g l list1 list2 = addGroups (a,b) lib g (l-1) (list1++(fst addTo)) (list2++(snd addTo))
  where
    beside = g!!(l-1)
    addTo  = if Lcom.find (==(a,b)) (snd(snd(lib!!beside))) /= Nothing then (snd(lib!!beside)) else ([],[])
--finds the liberty of diffrent stones by postion of the stones and sending those positions as a list of touples
addLiberty :: Int -> [(Int,Int)] -> (Int,Int)->[(Int,Int)]->Int->[(Int,Int)]
addLiberty _ _ (_,_) list 0 = list
addLiberty x pos (a,b) list p = if addTo /= [] then if (fst(head addTo))>=0 && (snd(head addTo))>=0 && (fst(head addTo))<=x && (snd(head addTo))<=x then addLiberty x pos (a,b) ((list)++(addTo)) (p-1)
                                        else addLiberty x pos (a,b) list (p-1)
                                    else addLiberty x pos (a,b) list (p-1)
  where
    beside = libPos (a,b)
    addTo  = if Lcom.find (==(beside!!(p-1))) pos == Nothing then [beside!!(p-1)] else []
--simple function for addLiberty to just "map out" the postion around a stone
libPos :: (Int,Int)->[(Int,Int)]
libPos (a,b) = [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]

--finds out if atleast one other stone of same color is around it
getOther :: World->Int->Int->Maybe Color->(Bool,(Int,Int))
getOther w a b c
              | b/=0 && Just(twoD!!a!!(b-1)) == Just c = (True,(a,b-1))
              | b/= ((boardSize w)-1) && Just(twoD!!a!!(b+1)) == Just c = (True,(a,b+1))
              | a/=0 && Just(twoD!!(a-1)!!b) == Just c = (True,(a-1,b))
              | a/= ((boardSize w)-1) && Just(twoD!!(a+1)!!b) == Just c = (True,(a+1,b))
              | otherwise       = (False, (a,b))
  where
    twoD = (board w)

--checks if the placement was legal without being taken by the opposite color stone
ifLegal :: [(Color,[(Int, ([ListPosition],[ListPosition]))])]->World->Color->Bool
ifLegal lib w c
            | c == White && checkLib (snd(head lib)) (length(snd(head lib))) w c = True
            | c == Black && checkLib (snd(last lib)) (length(snd(last lib))) w c = True
            | otherwise = False

--goes through all liberties of opposite color and gets a list of all postions in that list that
--and is compared to the length of opposite color to determine if its a legal move or not
checkLib :: [(Int, ([ListPosition],[ListPosition]))] -> Int -> World -> Color -> Bool
checkLib _ 0 _ _ = True
checkLib lib1 l w c
            | ((checkLib2 (snd(snd(lib1!!(l-1)))) (length (snd(snd(lib1!!(l-1))))) w c 0)==(length (snd(snd(lib1!!(l-1)))))) = False
            | otherwise = checkLib lib1 (l-1) w c

checkLib2 :: [ListPosition] -> Int -> World -> Color -> Int -> Int
checkLib2 _ 0 _ _ i = i
checkLib2 chosenlib l w c i
            | not ((board w)!!(fst positionsList)!!(snd positionsList)== Just (flipColor c)) = checkLib2 chosenlib (l-1) w c i
            | otherwise = checkLib2 chosenlib (l-1) w c (i+1)
  where
    positionsList = chosenlib!!(l-1)


--returns group number and group position in the data
getGroupNum :: [(Int,([(Int,Int)],[(Int,Int)]))]->(Int,Int)->Int->(Int,Int)
getGroupNum _ (_,_) 0 = (-1,-1)
getGroupNum lib (a,b) c = if  boardPlace /= Nothing then (fst(lib!!(c-1)),(c-1))
                          else getGroupNum lib (a,b) (c-1)
  where
    boardPlace = Lcom.find (==(a,b)) (fst(snd(lib!!(c-1))))
-- takes the group that you have list position and the group thingy for the color and lenght of the list and empty list
checkGroupToGroup :: Int-> [(Int, ([ListPosition], [ListPosition]))]->(Int,Int)->Int->[Int]->[Int]
checkGroupToGroup _ _ (_,_) 0 list = list
checkGroupToGroup g lib (a,b) p list 
                      | p==g = checkGroupToGroup g lib (a,b) (p-1) list
                      | boolGroup = checkGroupToGroup g lib (a,b) (p-1) (list++[p-1])
                      | otherwise = checkGroupToGroup g lib (a,b) (p-1) list
  where
    currentGroup = snd(snd(lib!!(p-1)))
    checkGroup = Lcom.findIndices (==(a,b)) currentGroup
    boolGroup = if checkGroup /= [] then True else False

--checks if the stone placed now would take any stones 
takeStone :: [(Int, ([ListPosition],[ListPosition]))]-> Int -> World -> Color-> (Bool,(Int,([ListPosition],[ListPosition])))
takeStone _ 0 _ _ = (False,(-1,([],[])))
takeStone lib l w c
            | ((takeStone2 (snd(snd(lib!!(l-1)))) (length (snd(snd(lib!!(l-1))))) w c 0)==(length (snd(snd(lib!!(l-1)))))-1) = (True,(lib!!(l-1)))
            | otherwise = takeStone lib (l-1) w c
            

takeStone2 :: [ListPosition] -> Int -> World -> Color -> Int -> Int
takeStone2 _ 0 _ _ i = i
takeStone2 chosenlib l w c i
            | not ((board w)!!(fst positionsList)!!(snd positionsList)== Just  c) = takeStone2 chosenlib (l-1) w c i
            | otherwise = takeStone2 chosenlib (l-1) w c (i+1)
  where
    positionsList = chosenlib!!(l-1)