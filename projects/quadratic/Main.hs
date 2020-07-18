{-- 
 - Author: haym 
 -
 - Generates quadratic attractors through the lettering scheme specified by
 - http://mathworld.wolfram.com/StrangeAttractor.html
--}

import Data.Char
import Data.Colour.Names
import System.Environment
import System.Process

import Lib

main :: IO ()
main = do
    args <- getArgs
    let (steps, letters) = case args of
            [] -> (50000, "FIRCDERRPVLD")
            [ls] -> (50000, toUpper <$> ls)
            [ls, steps'] -> (read steps', toUpper <$> ls) :: (Int, String)
        xs = take steps $ iterate (runAttractor (attrFromLetters letters)) (0, 0)
        fileName = letters ++ show steps ++  ".svg"

    plotPointsColor purple fileName letters 0.7 xs

    callCommand $ "svg " ++ fileName
