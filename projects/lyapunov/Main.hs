import Data.Maybe
import Numeric.AD
import Numeric.Tools.Integration
import System.Environment

func :: Num a => a -> a
func x = 4*x*(1 - x)

func' :: Double -> Double
func' = head . grad (func . head) . pure

integ :: Double -> Double
integ x = (log . abs . func' $ x) / sqrt (x*(1 - x)) * pi

leBruteForce :: Double -> Double
leBruteForce x1 = (1 / n) * log (delta / epsilon)
    where
        dist = abs <$> zipWith (-) (iterate func x1) (iterate func (x1 + epsilon))
        n = last $ zipWith const [1..] (takeWhile (<delta) dist)

epsilon :: Double
epsilon = 0.00000001

delta :: Double
delta = 0.1

main :: IO ()
main = do
    args <-  getArgs
   
    let x0 = maybe 0.7 read (listToMaybe args)
        xs = drop 1000 $ iterate func x0

    putStrLn $ unwords ["Using brute force with x1:", show (head xs), "eps:", show epsilon]
    putStrLn $ "L = " ++ show (leBruteForce $ head xs)

    putStrLn "\nUsing avg. derivative with same x1" 
    let lgrad = (sum . fmap (log . abs . func') $ take 10000 xs) / 10000
    putStrLn $ "L = " ++ show lgrad
    
    let res = quadTrapezoid defQuad (epsilon, 1 - epsilon) integ
    putStrLn $ "Integral = " ++ show (quadBestEst res)
