import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Lib

type World = [Double]

myGen :: StdGen
myGen = mkStdGen 42

w0 :: World
w0 = randomRs (-1, 1) myGen

main :: IO ()
main = playIO (InWindow "Nice Window" (1000, 1000) (10, 10)) 
    black 0 w0 (pure . mkPicture) newPoints (\_ w -> pure w)

mkPicture :: World -> Picture
mkPicture w = Pictures $ [(x, y) | x <- [-1,-0.9..1], y <- [-1,-0.9..1]] >>= go white
    where
        go c start =  take 32 . fmap (drawPoint c . snd) $ 
            iterate (rkStep (mkEq w) 0.05) (0, start)
        drawPoint c (x, y) = Color c . 
            Translate (500*realToFrac x) (500*realToFrac y) $ circleSolid 1

mkEq :: [Double] -> (Double, Double) -> (Double, Double)
mkEq (a:b:c:d:_) (x, y) = (x', y')
    where
        x' = a*x + b*y
        y' = c*x + d*y
mkEq _ _ = error "List must be infinite!"

newPoints :: Event -> World -> IO World
newPoints (EventKey (MouseButton _) Down _ _) xs = 
    print (take 4 xs) >> return (drop 4 xs)
newPoints _ w = return w
