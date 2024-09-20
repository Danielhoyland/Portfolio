module Lib
    ( someFunc
    , mainApp
    ) where


import qualified SDL
import qualified Common  as C
import           Data.Foldable  (foldl')
import           Control.Monad  (when)
import Data.Maybe (fromJust, isJust)


-- | Dummy function, returns "Hello " concatenated with a passed argument.
-- 
--
-- >>> someFunc "Dave"
-- "Hello Dave"
someFunc :: String -> String
someFunc name = "Hello " ++ name


type Position = (Int, Int)
data Color = Black | White deriving (Show, Eq)
type StoneGroup = (Color, [Position])

-- Intents
data Intent
  = Idle
  | Quit
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
  }


-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp ::  Int -> SDL.Window -> IO ()
mainApp x w =
    C.withRenderer w $ \r -> do
        -- lets load the background texture
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
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
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

startDragging :: World -> World
startDragging w = if stonePicked then w { worldDragging = True, worldDraggedColor = colorPicked } else w
  where
    selectedColor   = checkUserColorSelection
    stonePicked     = isJust selectedColor
    colorPicked     = fromJust selectedColor
    checkUserColorSelection
        | mx+q >= wx && my+q >= wy && mx+q <= wx + size && my+q <= wy + size = Just White
        | mx+q >= bx && my+q >= by && mx+q <= bx + size && my+q <= by + size = Just Black
        | otherwise = Nothing
        where
          size = squareSize w
          (mx, my) = worldMousePos w
          (wx, wy) = worldWhiteStoneXY w
          (bx, by) = worldBlackStoneXY w
          q = stoneRad w

stopDragging :: World -> World
stopDragging w
    | worldDragging w = w {
           worldDragging = False
         , worldStones = updatedStones }
    | otherwise = w
  where
    draggedColor            = worldDraggedColor w
    positions               = fromJust $ lookup draggedColor (worldStones w)
    theOtherGroupPositions  = fromJust $ lookup (flipColor draggedColor) (worldStones w)
    updatedStones           = (flipColor draggedColor, theOtherGroupPositions):[(draggedColor, placement)]
    mouseXY                 = worldMousePos w
    placement               = boardPos w mouseXY:positions 


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
                               }
  where
    gameDim = boardSize w
    maxRectSizeF = min ((fromIntegral width::Double) * (0.8::Double) /
                       (fromIntegral gameDim::Double)) (fromIntegral height * 0.7 / fromIntegral gameDim)
    rectSizeF = min maxRectSizeF (min (fromIntegral width) (fromIntegral height) / fromIntegral gameDim)
    xOffset = min ((0.1::Double) * fromIntegral width) (0.1 * fromIntegral height)
    whiteStonePrototypeXY = (round (rectSizeF * (fromIntegral gameDim + 2)), round (2 * xOffset))
    blackStonePrototypeXY = (round (rectSizeF * (fromIntegral gameDim + 2)), round (3 * xOffset))





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
drawEmptyGameBoard r gameDim offset square w = do
    -- color hardcoded here
    SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
    SDL.drawRect r . Just $ C.mkRect xy xy (wh*(p-1)) (wh*(p-1))
    drawLines r xy wh wh p p
  where
    xy      = offset - 1
    sqDim   = (square * gameDim) + 4
    wh      = ((p*square)) `div` p
    p       = boardSize         w

stoneRad :: World ->Int
stoneRad w = round ((fromIntegral (squareSize w)) / 2 :: Double)

boardPos ::  World -> (Int,Int)->(Int, Int)
boardPos w (x,y) = ((xPos x xy wh wh p),(yPos y xy wh wh p))
  where 
    xy      = offset - 1
    wh      = ((p*square)) `div` p
    offset  = xyOffset          w
    p       = boardSize         w
    square  = squareSize        w

xPos :: Int -> Int -> Int -> Int -> Int -> Int
xPos x offset xy yx p
  | xy>yx*p = x
  | x >= (yx `div` 2) && x < (yx `div` 2) + xy = offset + xy - yx - (yx `div` 2)
  | otherwise = xPos x offset (xy + yx) yx p

yPos :: Int -> Int -> Int -> Int -> Int -> Int
yPos y offset xy yx p
  | xy>yx*p = y
  | y >= (yx `div` 2) && y < (yx `div` 2) + xy = offset + xy - yx - (yx `div` 2)
  | otherwise = yPos y offset (xy + yx) yx p


drawLines::SDL.Renderer -> Int -> Int -> Int -> Int -> Int -> IO()
drawLines r xy wh yx p 2 = do return()
drawLines r xy wh yx p q = do 
  SDL.drawLine r (C.mkPoint (xy+wh) xy) (C.mkPoint (xy+wh) (xy+(yx*(p-1))))
  SDL.drawLine r (C.mkPoint xy (xy+wh)) (C.mkPoint (xy+(yx*(p-1))) (xy+wh))
  drawLines r xy (wh+yx) yx p (q-1)
