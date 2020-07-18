import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import System.Environment
import System.Process

import Lib

predator :: Double -> (Double, Double) -> (Double, Double)
predator beta (x, y) = (x', y')
    where
        x' = 1.1*x - 0.4*x*y
        y' = -0.4*y + beta*x*y

predatorPoints :: Double -> (Double, Double) -> ([(Double, Double)], [(Double, Double)])
predatorPoints beta s0 = unzip . fmap (\(t, (x, y)) -> ((t, x), (t, y))) $ xs
    where
        xs = take 2000 $ iterate (rkStep (predator beta) 0.05) (0, s0)

main = do
    args <- getArgs
    let p@[prey0, predator0, beta] = case args of
            xs@[p1, p2, b] -> read <$> xs
            xs@[p1, p2] -> fmap read xs ++ [0.5]
            _ -> [10, 10, 0.1]
        (preys, predators) = predatorPoints beta (prey0, predator0)

    toFile def "predator.svg" $ do
        layout_title .= show p
        plot (line "prey" . pure $ preys)
        plot (line "predator" . pure $ predators)

    callCommand "inkview predator.png"
