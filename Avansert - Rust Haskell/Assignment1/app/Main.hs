{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import qualified Lib


main :: IO ()
main = do
  --x <- getLine
  C.withSDL $ C.withSDLImage $ do
    C.setHintQuality
    C.withWindow "SDL Mouse Drag&Drop" (640, 480) (Lib.mainApp (9))

