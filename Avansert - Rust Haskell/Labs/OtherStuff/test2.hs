{-# LANGUAGE OverloadedStrings #-}
module Lib (sdl2Hello) where

import Data.Text (Text)
import SDL
import SDL.Font
import Control.Monad (unless)

gray :: SDL.Font.Color
gray = SDL.V4 128 128 128 255

myRenderer :: RendererConfig
myRenderer = RendererConfig
  { rendererType          = SoftwareRenderer  -- AcceleratedRenderer
  , rendererTargetTexture = True
  }


-- | Bootstraps our application and draws a window.
sdl2Hello :: IO ()
sdl2Hello = do
  initializeAll
  SDL.Font.initialize
  window <- createWindow "My Hello SDL2 Application" defaultWindow {windowResizable = True}
  renderer <- createRenderer window (-1) myRenderer
  appLoop renderer
  destroyWindow window
  SDL.Font.quit

-- | Main event handler and rendering loop.
appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  drawContent renderer
  unless qPressed (appLoop renderer)


-- | Hack to draw some text
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
        SDL.freeSurface textSurf


-- | Main function to draw the content on the screen
drawContent :: Renderer -> IO ()
drawContent r = do
  rendererDrawColor r $= V4 255 255 255 255 -- WHITE background
  clear r
  rendererDrawColor r $= V4 0 255 0 255 -- GREEN rect
  drawRect r $ Just $ SDL.Rectangle (P (V2 10 10)) (V2 400 400)
  rendererDrawColor r $= V4 255 0 0 255 -- RED rect
  drawRect r $ Just $ SDL.Rectangle (P (V2 60 60)) (V2 200 200)
  fillRect r $ Just $ SDL.Rectangle (P (V2 18 500)) (V2 30 30)
  drawText r "Press Q to quit" (10, 500)
  -- present updates the actual screen
  present r