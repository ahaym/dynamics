module Main where

import Numeric.AD
import Numeric.GSL.Integration
import Numeric.GSL.Minimization
import Numeric.GSL.Root

import Lib

m, l, g, w0, w1 :: (Num a, Floating a, Enum a) => a
m = 1.0
l = 1.0
g = 9.8
w0 = sqrt (g / l)
w1 = (4 / 5)*w0

lagrangian :: (Num a, Floating a, Enum a) => a -> a -> a -> a
lagrangian theta theta' t = (m / 2)*(l*theta')^2 + m*g*l*(cos theta)

freq1 :: Int -> (Double -> Double, [Double], Double)
freq1 n = (func, minAction, action)
    where
    (minAction, _) = minimize NMSimplex2
       0.01 500 (replicate n 100) mkAction $ replicate n 1
    func = mkTheta minAction
    action = mkAction minAction

    lagrangian_ thetaFunc theta'Func t =
        let
            theta = thetaFunc t
            theta' = theta'Func t
        in lagrangian theta theta' t
    
    mkTheta as t = let go a n = a*sin ((2*n - 1)*w1*t)
        in sum $ zipWith go as [1..]
    mkTheta' as t = let go a n = (2*n - 1)*w1*a*cos ((2*n - 1)*w1*t)
        in sum $ zipWith go as [1..]

    mkAction as =
        let
            theta =  mkTheta as
            theta' = mkTheta' as
        in integrate (lagrangian_ theta theta')

    integrate f = fst $ integrateQAGS 1e-2 1000 f 0 50

freq2 :: (Double, Double, [(Double, Double, Double)])
freq2 = (ans, frequency ans, points ans)
    where
    (ans, _) = uniRoot Brent 0.001 10 (\x -> frequency x - w1) 3 5

    lagrangian_ [x, v] = lagrangian x v 0
    accel x v = 
        let
            state = [x, v]
            [f, p] = grad lagrangian_ state
            hes = hessian lagrangian_ state
            d1p = hes !! 1 !! 0
            d2p = hes !! 1 !! 1
            d0p = 0
        in (f - (d0p + d1p*v)) / d2p
   
    points theta' = iterate (eulerStep' accel 0.001) (0, 0, theta')

    frequency theta' = 
        let
            ps = points theta'
            zipped = zipWith (\(t0 ,x0,_) (_,x1,_) -> (t0, x0, x1)) ps (tail ps)
            zeroes = filter (\(_, x0, x1) -> x0 == 0 || (x0 <= 0 && x1 >= 0)) zipped
            (tn, _, _) = zeroes !! 1
        in 2*pi / tn

freq3 :: (Double, Double)
freq3 = (ans, 2*pi / period ans)
    where
    (ans, _) = uniRoot Brent 1e-5 100 (\x -> period x - t) 1 3
    t = (2*pi) / w1
    theta' amp theta = sqrt ((2*g*(cos theta - cos amp)) / l)
    func amp theta = 4 / (theta' amp theta)
    period amp = fst $ integrateQAGS 1e-6 1000 (func amp) 0 amp

main :: IO ()
main = do
    putStrLn "problem a action, weights"
    print action
    print nums
    plotPoints "1a.svg" "pendulum (radians)" 1 $ [(t, func t) | t <-[0,0.01..25]]
    putStrLn "problem b v0, frequency"
    print v0
    print freq
    plotPoints "1b.svg" "pendulum (radians)" 1 $ 
        take 5000 $ map (\(t,x,_v) -> (t,x)) numsb
    putStrLn "problem c (amplitude, frequency)"
    where
    (func, nums, action) = freq1 10
    (v0, freq, numsb) = freq2
