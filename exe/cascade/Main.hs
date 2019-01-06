module Main where

import System.Process

import Lib

logistic :: Double -> Double -> Double
logistic r x = r*x*(1 - x)

iterLog :: Double -> [(Double, Double)]
iterLog r = map (\x -> (r, x)) . drop 100 . take 500 $ iterate (logistic r) 0.7

xs :: [(Double, Double)]
xs = [2.5, 2.5+0.001..4] >>= iterLog

main = do
    plotPoints "cascade.png" "Feigenbaum Cascade" 0.5 xs
    callCommand "feh cascade.png"
