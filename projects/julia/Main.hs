import Data.Complex
import System.Environment
import System.Process

import Lib

func c z = z^2 + c

phi = (1 + sqrt 5) / 2

main = do
    args <- getArgs
    
    let real:imag:_ = if null args then [1 - phi, 0] else read <$> args

        points = (:+) <$> [-1.5,-1.495..1.5] <*>  [-1.2,-1.195..1.2]
        jset = c2t <$> filter (not . diverges 50 (func (real :+ imag))) points

        fileName = "julia" ++ show real ++ "-" ++ show imag ++ ".svg"
        title = "Julia Set for z^2 + " ++ show real ++ " + " ++ show imag ++ "i"

    plotPoints fileName title 1.7 jset
    callCommand $ "inkview " ++ fileName
