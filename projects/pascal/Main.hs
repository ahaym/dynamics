import Data.Function
import Lib

pt2 = fix $ \xss -> [1] : map (\xs -> zipWith xor (0:xs) (xs ++ [0])) xss
kpoints = map (filter (/=0) . zipWith (*) [1..]) pt2
points = zipWith (\a xs -> map (\x -> (a, x)) xs) [0..] kpoints

xor a b = if a == b then 0 else 1

main = plotPoints' "pascal.png" "Pascal's Triangle Mod 2" 0.5 $ concat (take 6000 points)
