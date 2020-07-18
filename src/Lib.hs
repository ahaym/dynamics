{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{--
 - Author: haym 
 -
 - Helper functions common to multiple projects
--}

module Lib where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Colour.Names
import Data.Maybe

import Data.Complex
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import System.Process

type C = Complex Double

class Mag a where
    mag :: a -> Double

instance Mag Double where
    mag = id

instance Mag C where
    mag = magnitude

plotPoints' :: FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPoints' file title rad xs = plotPoints file title rad xs >> callCommand ("inkview " ++ file)

plotPoints :: FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPoints = plotPointsColor black

plotPointsColor :: Colour Double -> FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPointsColor color file title rad xs = toFile def file $ do
        layout_title .= title
        plot (pointsConfig color rad <$> points "points" xs)

pointsConfig :: Colour Double -> Double -> PlotPoints x y -> PlotPoints x y 
pointsConfig color rad
    = set (plot_points_style.point_color) (opaque color)
    . set (plot_points_style.point_radius) rad

diverges :: (Mag a) => Int -> (a -> a) -> a -> Bool
diverges maxIters func x0 = any (\x -> mag x > 4) . take maxIters $ iterate func x0

c2t :: Complex Double -> (Double, Double)
c2t (r :+ i) = (r, i)

letterCoeff :: Char -> Double
letterCoeff c = fromIntegral (fromEnum (toLower c) - fromEnum 'a') / 10 - 1.2

type Attractor = (Double -> Double -> Double, Double -> Double -> Double)

mkAttractor :: [Double] -> Attractor
mkAttractor cs = (fx, fy)
    where
        getNum = get >>= \case
            (x:xs) -> put xs >> return x
            _ -> return 0
        mkMap = do
            n <- replicateM 6 getNum
            return $ case n of
                [a1, a2, a3, a4, a5, a6] ->
                    let f x y = a1 + a2*x + a3*x*x + a4*x*y + a5*y + a6*y*y
                    in f
                [] -> error "lol"

        [fx, fy] = evalState (replicateM 2 mkMap) cs

attrFromLetters :: String -> Attractor
attrFromLetters = mkAttractor . fmap letterCoeff

runAttractor :: Attractor -> (Double, Double) -> (Double, Double)
runAttractor (fx, fy) (x, y) = (fx x y, fy x y)

class VSpace a where
    (^+) :: a -> a -> a
    (^*) ::  Double -> a -> a

instance VSpace Double where
    (^+) = (+)
    (^*) = (*)

instance VSpace (Double, Double) where
    (^+) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1) 
    (^*) a (x, y) = (a*x, a*y) 

instance VSpace (Double, Double, Double) where
    (^+) (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1) 
    (^*) a (x, y, z) = (a*x, a*y, a*z) 

rkStep :: (VSpace a) => (a -> a) -> Double -> (Double, a) -> (Double, a)
rkStep f dt (t, y) = (t + dt, y ^+ ((dt / 6)^*(k1 ^+ (2.0^*k2) ^+ (2.0^*k3) ^+ k4)))
    where
        k1 = f y
        k2 = f (y ^+ ((0.5*dt)^*k1))
        k3 = f (y ^+ ((0.5*dt)^*k2))
        k4 = f (y ^+ (dt^*k3))

rkStep' :: (VSpace a) => (a -> a -> a) -> Double -> (Double, a, a) -> (Double, a, a)
rkStep' f dt (t, y, y') = (t + dt, y ^+ (dt^*y'), y'New)
    where
        y'New = y' ^+ ((dt / 6)^*(k1 ^+ (2.0^*k2) ^+ (2.0^*k3) ^+ k4))
        k1 = f y y'
        k2 = f y (y' ^+ ((0.5*dt)^*k1))
        k3 = f y (y' ^+ ((0.5*dt)^*k2))
        k4 = f y (y' ^+ (dt^*k3))

eulerStep' :: VSpace a => (a -> a -> a) -> Double -> (Double, a, a) -> (Double, a, a)
eulerStep' a dt (t, y, y') = (tNew, yNew, y'New)
    where
    y'New = y' ^+ (dt^*a y y')
    yNew = y ^+ (dt^*y'New)
    tNew = t + dt

eulerStep :: (VSpace a) => (a -> a) -> Double -> (Double, a) -> (Double, a)
eulerStep f dt (t, y) = (t + dt, y ^+ (dt^*f y))
