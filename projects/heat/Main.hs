import Control.Arrow
import Control.Monad
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Environment
import System.Random

import Lib

type World = M.Map (Int, Int) Float

eps = 0.3

drawSquare ((x, y), val) = Color c . Translate (10*x - 500) (10*y - 500) $ rectangleSolid 10 10
    where
        c = makeColor 1 0 0 (val / 100)

hot = (,) <$> [50] <*> [45..55]

myGen :: StdGen
myGen = mkStdGen 42

rands = randomRs (0, 100) myGen

m0 = m'
    where
        m = M.fromList . flip zip rands $  
            ((,) <$> [0..100] <*> [0..100])
        go0 m p = M.insert p 0 m 
        m' = addHeat hot m

addHeat xs m = foldl go m xs
    where
        go m p = M.insert p 100 m

lookHeat m p = fromMaybe 0 (M.lookup p m)

stepHeat m = addHeat hot $ M.mapWithKey (stepHeat1 m) m

stepHeat1 m (x, y) v = v'
    where
        v' = (1-eps)*v + eps*sum (map (lookHeat m) ps) / 9
        ps = (,) <$> [x - 1..x + 1] <*> [y - 1..y + 1]

mkPicture worldRef lastTimeRef secs = do
    pic <- Pictures . map drawSquare . M.toList <$> readIORef worldRef
    w <- readIORef worldRef
    lastTime <- readIORef lastTimeRef
    when (secs - lastTime > 0.01) $ do
        writeIORef lastTimeRef secs
        modifyIORef worldRef stepHeat
    return pic

main :: IO ()
main = do
    worldRef <- newIORef m0
    lastTimeRef <- newIORef 0.0
    animateIO (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        white (mkPicture worldRef lastTimeRef) (const $ return ())
