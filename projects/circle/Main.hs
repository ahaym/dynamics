import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate

ts = [0, 0.05..2*pi]

xs = const 0 <$> ts

ys = sin <$> ts

zs = cos <$> ts

mkPicture secs = Color white . Line $ zip ys' zs
    where
        ys' = zipWith (-) (fmap (*cos secs) xs) (fmap (*sin secs) ys)
        zs' = zipWith (+) (fmap (*sin secs) xs) (fmap (*cos secs) ys)

drawPoint r c x y = Color c . 
    Translate (500*realToFrac x) (500*realToFrac y) $ circleSolid r

main :: IO ()
main = animate (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        black mkPicture

