import Data.Complex

import Lib

func c z = z^2 + c

diverge maxIters f x0 = (any (\x -> magnitude x > 4) . take maxIters) (iterate f x0)

points = [a :+ b | a <- [-2.5,-2.495..1], b <- [-1,-0.995..1]]

mset = filter (\x -> not (diverge 1000 (func x) 0)) points

main = plotPoints' "mandelbrot.svg" "Mandelbrot Set" 1.4 (map c2t mset)
