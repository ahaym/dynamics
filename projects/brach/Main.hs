{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Data.Maybe (listToMaybe)
import Data.Reflection (Reifies)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Numeric.AD
import Numeric.AD.Internal.Reverse (Tape, Reverse)
import Numeric.AD.Newton
import System.Environment
import System.Process (callCommand)

import Lib

zipB :: (a, b) -> (a, b) -> [(a, b)] -> [((a, b), (a, b))]
zipB start end points = go (start:points)
    where
        go [] = []
        go [x] = [(x, end)]
        go (x0:x1:xs) = (x0,x1) : go (x1:xs)

foldB :: (Double, Double) -> (Double, Double) -> [(Double, Double)] -> ([(Double, Double)], Double)
foldB start end points = (start : ans ++ [end], sum . map score . zipB start end $ ans)
    where
        ans = zip xs (gradientDescent go ys !! 5000)
        (x0, y0) = start
        (x1, y1) = end
        (xs, ys) = unzip points

        go :: forall s. Reifies s Tape => [Reverse s Double] -> Reverse s Double
        go = sum . map score . zipB (auto x0, auto y0) (auto x1, auto y1) 
            . zip (auto <$> xs)

        score :: (Num a, Floating a, Mode a) => ((a, a), (a, a)) -> a
        score ((x0, y0), (x1, y1)) = 2*top / bottom
            where
                top = sqrt $ (x1 - x0)^2 + (y1 - y0)^2
                bottom = sqrt (1 - y1) + sqrt (1 - y0)

main :: IO ()
main = do 
    yN <- maybe 1 read . listToMaybe <$> getArgs
    let (ps, ans) = foldB (0, 1) (1, yN) [(x, 1 - x) | x <- [0.01,0.02..0.99]]
        --points' = [(x, f x) | x <- [0, 0.01..1]]
        --f x = (4*(1 - 0.62302128))*(x - 0.5)^2 + 0.62302128
    print ans
    toFile def "brachGrad.png" $ do
        layout_title .= "Brachistochrone"
        plot (pointsConfig black 3 <$> points "points" ps)
        --plot (pointsConfig red 5 <$> points "parab" points')
    callCommand ("feh " ++ "brachGrad.png")
