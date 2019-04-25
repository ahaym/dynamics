import Graphics.Gloss.Interface.Pure.Game
import System.Environment

import Lib

type World = ((Float, Float), (Float, Float))

constraints x y = 
    30*x + 5*y >= 60 &&
    15*x + 10*y >= 70 &&
    x >= 0 &&
    y >= 0

mkPicture (xys, (c1, c2)) = Translate (-300) (-300) . Scale 20 20 .  Pictures $ ps
    where
        c1 = Line $ (\x -> (x, -6*x + 12)) <$> [-100, 100]
        c2 = Line $ (\x -> (x, -1.5*x + 7)) <$> [-100, 100]
        c3 = Line [(0, -100), (0, 100)] 
        c4 = Line [(-100, 0), (100, 0)] 
        pt (x, y) = Color blue $ Translate x y (circleSolid 0.2)
        ps = (pt <$> xys) ++ [c1, c2 ,c3, c4]

newPoints (EventKey (SpecialKey KeySpace) Down _ _) (xys, (c1, c2)) = (descend <$> xys, (c1, c2))
    where
        cost x y
            | constraints x y = c1*x + c2*y
            | otherwise = 10000000
        descend (x, y)
            | constraints (-0.4*c1 + x) (-0.4*c2 + y) = (-0.4*c1+ x, -0.4*c2 + y)
            | otherwise = snd $ minimum considered
            where
                ds = [(-0.4),(-0.375)..0.4]
                newp = [(x+dx, y+dy) | dx <- ds, dy <- ds]
                considered = (\(x, y) -> (cost x y, (x, y))) <$> newp

newPoints (EventKey (MouseButton _) Down _ (x_, y_)) (xys, cs) = (xy':xys, cs)
    where
        xy' = ((x_ + 300) / 20, (y_ + 300) / 20)

newPoints _ w = w

main = do
    args <- getArgs
    let [c1, c2] = case args of
            cs@[c1_, c2_] -> read <$> cs
            _ -> [29.9999, 5]
    play (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        white 0 ([], (c1 / c2, 1)) mkPicture newPoints (flip const)
