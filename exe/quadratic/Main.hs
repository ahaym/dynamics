{-# LANGUAGE LambdaCase #-}

{-- 
 - Author: haym 
 -
 - Generates quadratic attractors through the lettering scheme specified by
 - http://mathworld.wolfram.com/StrangeAttractor.html
--}

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Colour.Names
import Data.Maybe
import System.Environment
import System.Process

import Lib

letterCoeff :: Char -> Double
letterCoeff c = fromIntegral (fromEnum (toLower c) - fromEnum 'a') / 10 - 1.2

type Attractor = (Double -> Double -> Double, Double -> Double -> Double)

mkAttractor :: [Double] -> Attractor
mkAttractor cs = (fx, fy)
    where
        getNum = get >>= \case
            (x:xs) -> put xs >> return x
            _ -> return 0
        mkMap = do
            [a1, a2, a3, a4, a5, a6] <- replicateM 6 getNum
            let f x y = a1 + a2*x + a3*x*x + a4*x*y + a5*y + a6*y*y
            return f

        [fx, fy] = evalState (replicateM 2 mkMap) cs

attrFromLetters :: String -> Attractor
attrFromLetters = mkAttractor . fmap letterCoeff

runAttractor :: Attractor -> (Double, Double) -> (Double, Double)
runAttractor (fx, fy) (x, y) = (fx x y, fy x y)

main :: IO ()
main = do
    args <- getArgs
    let (steps, letters) = case args of
            [] -> (50000, "FIRCDERRPVLD")
            [ls] -> (50000, toUpper <$> ls)
            [ls, steps'] -> (read steps', toUpper <$> ls) :: (Int, String)
        xs = take steps $ iterate (runAttractor (attrFromLetters letters)) (0, 0)
        fileName = letters ++ show steps ++  ".png"

    plotPointsColor purple fileName letters 0.7 xs

    callCommand $ "feh " ++ fileName
