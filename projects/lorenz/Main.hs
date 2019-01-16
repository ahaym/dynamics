import Control.Arrow
import Control.Monad
import Data.IORef
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Environment

import Lib

type R3 = (Double, Double, Double)

rossler :: R3 -> R3
rossler (y, z, x) = (y', z', x')
    where
        (a, b, c) = (0.2, 0.2, 5.7)
        x' = -y - z 
        y' = x + a*y
        z' = b + z*(x - c)

lorenz :: R3 -> R3
lorenz (x, y, z) = (x', y', z')
    where
        (sigma, r, b) = (10, 28, 8 / 3)
        x' = sigma*(y - x)
        y' = r*x - y - x*z
        z' = x*y - b*z

points :: (R3 -> R3, Double) -> ([(Double, R3)], [(Double, R3)])
points (fun, step) = (iterate (rkStep fun step) (0, (1, 1, 1)), 
    iterate (rkStep fun step) (0, (1.0001, 1, 1)))

rate :: Int
rate = 5

mkPicture :: Int -> IORef ([(Double, R3)], [(Double, R3)]) -> IORef Int -> Float -> IO Picture
mkPicture maxPoints ps'io lastTime_ secs_ = do
    (ps1, ps2) <- readIORef ps'io
    lastTime <- readIORef lastTime_
    let [ps1', ps2'] = unzip3 . take toTake . fmap snd <$> [ps1, ps2]
        secs = realToFrac secs_ / 4
        msecs = floor $ secs_*1000
        curSize = msecs `div` rate
        toTake = if curSize < maxPoints then curSize else maxPoints
        mkLine ts@(_,ys,zs) = Line $ zip (realToFrac <$> rotY ts) (realToFrac <$> zs)
        rotY (xs,ys,zs) = zipWith (-) (fmap (*cos secs) xs) (fmap (*sin secs) ys)
        pics = Pictures [Color red (mkLine ps1'), Color blue (mkLine ps2')]

    when (msecs - lastTime > rate) $ do
        modifyIORef ps'io $ \pss -> 
            if curSize > maxPoints then (tail *** tail) pss
                else pss
        writeIORef lastTime_ msecs
    
    return . Scale 16 16 . Translate (-1) (-20) $ pics

main :: IO ()
main = do
    args <- getArgs
    let fun = case listToMaybe args of
            Just "rossler" -> (rossler, 0.05)
            _ -> (lorenz, 0.01)
        maxPoints = maybe 6000 read (listToMaybe $ drop 1 args)

    ps <- newIORef (points fun)
    curMS <- newIORef 0
    animateIO (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        white (mkPicture maxPoints ps curMS) (const $ return ())
