{-# LANGUAGE FlexibleInstances #-}

{-- 
 - Author: haym 
 -
 - Helper functions common to multiple projects
--}

module Lib where

import Data.Complex
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

type C = Complex Double

class Mag a where
    mag :: a -> Double

instance Mag Double where
    mag = id

instance Mag C where
    mag = magnitude

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
