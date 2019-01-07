{-- 
 - Author: haym 
 -
 - Helper functions common to multiple projects
--}

module Lib where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotPoints :: FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPoints = plotPointsColor purple

plotPointsColor :: Colour Double -> FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPointsColor color file title rad xs = toFile def file $ do
        layout_title .= title
        plot (pointsConfig color rad <$> points "points" xs)

pointsConfig :: Colour Double -> Double -> PlotPoints x y -> PlotPoints x y 
pointsConfig color rad
    = set (plot_points_style.point_color) (opaque color)
    . set (plot_points_style.point_radius) rad
