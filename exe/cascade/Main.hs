{-- 
 - Author: haym 
 -
 - Generates the Feigenbaum Cascade from 2.5 to 4
 - May take around a minute , as two million points are being plotted and
 - the plotting library doesn't seem too efficient.
--}

import System.Process

import Lib

logistic :: Double -> Double -> Double
logistic r x = r*x*(1 - x)

iterLog :: Double -> [(Double, Double)]
iterLog r = map (\x -> (r, x)) . drop 100 . take 1500 $ iterate (logistic r) 0.7

xs :: [(Double, Double)]
xs = [2.5, 2.5+0.001..4] >>= iterLog

main = do
    plotPoints "cascade.png" "Feigenbaum Cascade" 0.25 xs
    callCommand "feh cascade.png"
