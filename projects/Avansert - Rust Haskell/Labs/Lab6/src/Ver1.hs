{-# LANGUAGE OverloadedStrings #-}

module Ver1 (
    mainApp
) where


import qualified Common as C

import Control.Monad          
import Control.Monad.Loops    (iterateUntilM)
import Control.Monad.State    (modify)
import Data.Foldable          (foldl')
import SDL
import SDL.Font
import Data.Text (Text)
import Data.Text (pack)
import Data.IORef


gray :: SDL.Font.Color
gray = SDL.V4 128 128 128 255

data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant

data SPane = SPane {
    position :: (Double, Double),
    size :: (Int, Int)
}

data World = World
  { exiting :: Bool
  , panes   :: PaneMap
  }


data PaneMap = PaneMap
  { topLeft     :: Pane
  , topRight    :: Pane
  , bottomLeft  :: Pane
  , bottomRight :: Pane
  }


data Pane
  = Out
  | Over
  | Down
  | Up


data Quadrant
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight


initialWorld :: World
initialWorld = World
  { exiting = False
  , panes = initialPanes
  }


initialPanes :: PaneMap
initialPanes = PaneMap
  { topLeft     = Out
  , topRight    = Out
  , bottomLeft  = Out
  , bottomRight = Out
  }

-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp w =
    C.withRenderer w $ \r -> do
      initializeAll
      SDL.Font.initialize
      t <- C.loadTextureWithInfo r "./assets/rock.png"

      -- we create an utility curry for us here
      let doRender = Ver1.renderWorld r t

      -- we could also write the next line like that:
      -- _ <- iterateUntilM
      void $ iterateUntilM
        Ver1.exiting
        (\xw ->
          -- How does this line work?
             SDL.pollEvents >>= (\xw' -> xw' <$ doRender xw') . updateWorld xw

          --
          -- Below is how we would add a delay, same as in the Rust implementation
          -- for "slowing down" the loop to less than 30 frames per second
          --
          -- SDL.pollEvents >>= (\xw' -> threadDelay (1000000 `div` 30) >> xw' <$ doRender xw') . updateWorld xw
          --
          -- for the above line to work, do also at the top of the source file:
          -- import Control.Concurrent     (threadDelay)
          --
        )
        {-- alternative formulation
                  Lib.updateWorld xw <$> SDL.pollEvents
                  >>= \xw' -> xw' <$ doRender xw'
         -}
        Ver1.initialWorld

      -- when we are done with the renderer, we need to clean up
      SDL.destroyTexture (fst t)


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
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle

-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = Hover q
  where
    q = selectQuadrant x y
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
  --
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = t q
  where
    q = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    t = if SDL.mouseButtonEventMotion e == SDL.Pressed
           then Press
           else Release


selectQuadrant :: (Num a, Ord a) => a -> a -> Quadrant
selectQuadrant x y
  | x <  320 && y <  240 = TopLeft
  | x >= 320 && y <  240 = TopRight
  | x <  320 && y >= 240 = BottomLeft
  | x >= 320 && y >= 240 = BottomRight
  | otherwise            = undefined


applyIntent :: Intent -> World -> World
applyIntent (Press q)   = pressWorld q
applyIntent (Release q) = releaseWorld q
applyIntent (Hover q)   = hoverWorld q
applyIntent (Leave q)   = leaveWorld q
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


updatePaneMap :: (Pane -> Pane) -> (Pane -> Pane) -> Quadrant -> PaneMap -> PaneMap
updatePaneMap f g TopLeft     (PaneMap tl tr bl br) = PaneMap (f tl) (g tr) (g bl) (g br)
updatePaneMap f g TopRight    (PaneMap tl tr bl br) = PaneMap (g tl) (f tr) (g bl) (g br)
updatePaneMap f g BottomLeft  (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (f bl) (g br)
updatePaneMap f g BottomRight (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (g bl) (f br)


pressWorld :: Quadrant -> World -> World
pressWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setDown id q (panes w)


releaseWorld :: Quadrant -> World -> World
releaseWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setUp id q (panes w)


hoverWorld :: Quadrant -> World -> World
hoverWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOver setOut q (panes w)


leaveWorld :: Quadrant -> World -> World
leaveWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOut setOver q (panes w)


setOut :: Pane -> Pane
setOut _ = Out


setOver :: Pane -> Pane
setOver Down = Down
setOver Up = Up
setOver _ = Over


setDown :: Pane -> Pane
setDown _ = Down


setUp :: Pane -> Pane
setUp _ = Up


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }

-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window, 
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  (mx, my) <- getPositionXY
  mouseButtons <- getMouseButtons
  drawWorld r t (mx, my) w mouseButtons
  SDL.present r

-- The actual method for drawing that is used by the rendering method above.
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



-- Converts Pane to the texture coordinates.
getMask :: (Num a) => Pane -> (a, a)
getMask Out  = (  0,   0)
getMask Over = (320,   0)
getMask Down = (  0, 240)
getMask Up   = (320, 240)

-- Returns the coordinates of upper-left corners of specific Quadrants.
getPosition :: (Num a) => Quadrant -> (a, a)
getPosition TopLeft     = (  0,   0)
getPosition TopRight    = (320,   0)
getPosition BottomLeft  = (  0, 240)
getPosition BottomRight = (320, 240)

-- | Let us draw some text
drawText :: Renderer -> Text -> (Int, Int) -> IO ()
drawText r t (x, y) = do
        font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 70
        textSurf <- SDL.Font.solid font gray t
        SDL.Font.free font
        surf <- createTextureFromSurface r textSurf
        info <- queryTexture surf
        let w = textureWidth info
        let h = textureHeight info
        SDL.copy r surf Nothing (Just $ SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 w h))
        SDL.destroyTexture surf
        SDL.freeSurface textSurf

getPositionXY :: IO (Int, Int)
getPositionXY = do
  P (V2 x y) <- getAbsoluteMouseLocation
  return (fromIntegral x, fromIntegral y)

click :: Int-> Int -> Int -> Int -> Int -> Int -> Bool
click x y tw th mx my
  | x <= mx && mx <= (x + tw) && y <= my && my <= (y + th) = True
  | otherwise = False
