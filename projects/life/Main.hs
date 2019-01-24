import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Random

data Env = Env
    { world :: !(M.Map (Int, Int) Bool)
    , pause :: !Bool
    , lastTime :: !Float
    }

myGen = mkStdGen 42

rands = randomRs (False, True)

w0 g'm = M.fromList . flip zip vals $  ((,) <$> [0..100] <*> [0..100])
    where
        vals = maybe (repeat False) rands g'm

e0 g'm = Env (w0 g'm) True 0

mkPicture = Pictures . map drawSquare . M.toList . world

adj (x, y) = tail $ (,) <$> [x, x - 1, x + 1] <*> [y, y - 1, y + 1]

lookWorld m p = fromMaybe False (M.lookup p m)

drawSquare ((x_, y_), val) = Color c . Translate (10*x - 500) (10*y - 500) $ rectangleSolid 8 8
    where
        c = if val then black else white
        x = fromIntegral x_
        y = fromIntegral y_

stepWorld w = M.mapWithKey step1 w
    where
        step1 p v
            | neighbors < 2 = False
            | neighbors > 3 = False
            | neighbors == 3 = True
            | otherwise = v
            where
                neighbors = length . filter id $ lookWorld w <$> adj p

stepEnv _ e@Env{world = w, pause = isPaused}
    | isPaused = return e
    | otherwise = return e{world = stepWorld w}

main = do
    args <- listToMaybe <$> getArgs
    gen'm <- (args *>) . Just <$> getStdGen
    playIO (InWindow "Nice Window" (1000, 1000) (10, 10)) 
        rose 5 (e0 gen'm) (return . mkPicture) newPoints stepEnv

newPoints (EventKey (MouseButton _) Down _ (x_, y_)) e@Env{world = w} = return e{world = w'}
    where
        findCoord a = round $ (a + 500) / 10
        [x, y] = findCoord <$> [x_, y_]
        w' = M.adjust not (x, y)  w

newPoints (EventKey (SpecialKey KeySpace) Down _ _) e@Env{pause = p} = return e{pause = not p}

newPoints _ w = return w
