import Control.Monad
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Environment
import System.Random

eps = 0.09
deps = 0.15

f u v = rho*u*(1.0-u)*(u-(v+b)/a)
    where
        a = 0.75
        b = 0.0006
        rho = 17

g = flip (-)

drawSquare ((x, y), val) = Color c . Translate (10*x - 500) (10*y - 500) $ rectangleSolid 10 10
    where
        c = makeColor 0 0 1 val

myGen :: StdGen
myGen = mkStdGen 420

rands :: [Float]
rands = randomRs (0, 1) myGen

m0 = (mu, mv)
    where
        mu = M.fromList . flip zip rands $  
            ((,) <$> [0..99] <*> [0..99])
        mv = M.fromList . flip zip (take 10000 rands) $  
            ((,) <$> [0..99] <*> [0..99])

addBZ xs m = foldl go m xs
    where
        go ma p = M.insert p 0.95 ma

lookBZ m p = fromMaybe 0.95 (M.lookup p m)

stepBZ (mu, mv) = (muDiff,
   M.mapWithKey (stepBZ1 False g (mv, muDiff)) mv) 
    where
        muDiff = M.mapWithKey (stepBZ1 True f (mu, mv)) mu

stepBZ1 diffuse f (self, other) (x, y) v = v''
    where
        v' = if diffuse 
            then (1-deps)*v + deps*sum (map (lookBZ self) ps) / 4
            else v
        v'' = v' + eps*reaction
        reaction = f (lookBZ self (x, y)) (lookBZ other (x, y))
        ps = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

mkPicture worldRef lastTimeRef secs = do
    pic <- Pictures . map drawSquare . M.toList . fst <$> readIORef worldRef
    lastTime <- readIORef lastTimeRef
    when (secs - lastTime > 0.001) $ do
        writeIORef lastTimeRef secs
        modifyIORef worldRef stepBZ
    return pic

main :: IO ()
main = do
    worldRef <- newIORef m0
    lastTimeRef <- newIORef 0.0
    animateIO (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        green (mkPicture worldRef lastTimeRef) (const $ return ())
