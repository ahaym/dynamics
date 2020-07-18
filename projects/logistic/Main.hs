import Data.Maybe
import System.Environment

import Lib

logistic :: Double -> Double
logistic x = x*(1 - x)

logisticPoints :: Double -> [(Double, Double)]
logisticPoints x0 = take 1000 $ iterate (rkStep logistic 0.01) (0, x0)

main :: IO ()
main = do
    args <- getArgs
    let x0 = maybe 0.01 read $ listToMaybe args
        title = "x0 = " ++ show x0
    plotPoints' "logistic.svg" title 1 $ logisticPoints x0
