{-- 
 - Author: haym 
 -
 - Shows the divergence of two nearby points on a quadratic 2d strange attractor
 -
 - Generates quadratic attractors through the lettering scheme specified by
 - http://mathworld.wolfram.com/StrangeAttractor.html
--}

import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Environment

import Lib

type World = ((Double, Double), (Double, Double))

main :: IO ()
main = do
    args <- getArgs
    let attr = attrFromLetters $ if null args then "MDVAIDOYHYEA" else head args
    play (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        black 0 ((0, 0), (0, 0.00001)) (mkPicture attr) (newPoints attr) (flip const)

mkPicture :: Attractor -> World -> Picture
mkPicture attr (a, b) = Pictures $ pic ++ points
    where
        drawPoint r c (x, y) = Color c . 
            Translate (500*realToFrac x) (500*realToFrac y) $ circleSolid r
        pic = take 7000 $ drawPoint 1 white <$> iterate (runAttractor attr) (0, 0)
        points = uncurry (drawPoint 7) <$> [(red, a), (blue, b)]

newPoints :: Attractor -> Event -> World -> World
newPoints attr (EventKey _ Down _ _) (a, b) = (runAttractor attr a, runAttractor attr b)
newPoints _ _ w = w
