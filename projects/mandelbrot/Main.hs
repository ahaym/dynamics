import Data.Complex
import System.Environment
import System.Process

import Lib

func c z = z^2 + c

main = do
    let points = (:+) <$> [-2.5,-2.495..1] <*>  [-1,-0.995..1]
        mset = c2t <$> filter (\x -> not $ diverges 1000 (func x) 0) points
    plotPoints "mandelbrot.png" "Mandelbrot Set" 1.4 mset
    callCommand "feh mandelbrot.png"
