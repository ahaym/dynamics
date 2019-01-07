module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Nice Window" (1000, 1000) (10, 10)) white (translate (-500) (-500) $ circleSolid 80)
