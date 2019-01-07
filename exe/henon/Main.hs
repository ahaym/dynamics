{-- 
 - Author: haym 
 -
 - Plots the Henon map for some number of steps, starting at (0.5, 0.5).
 - Generates both a point and line plot
--}

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Lib
import System.Environment
import System.Process

henon :: Double -> Double -> (Double, Double) -> (Double, Double)
henon a b (x, y) = (x', y')
    where
        x' = 1 - a*(x^2) + y
        y' = b*x

main :: IO ()
main = do
    args <- getArgs
    let (steps, [a, b, x, y]) = case args of
            (stepStr:rest) -> (read stepStr :: Int, read <$> rest :: [Double])
            _ -> (300000, [1.4, 0.3, 0.5, 0.5])
        xs = take steps $ iterate (henon a b) (x, y)
        title = unwords ["Henon Map with a =", show a, "b =", show b, "init =", show (x, y)]
    
    plotPoints "henon.png" title 0.7 xs

    toFile def "henonLine.png" $ do
        layout_title .= title
        plot (line "henon" . pure . take 500 $ xs)

    callCommand "feh henon*.png"
