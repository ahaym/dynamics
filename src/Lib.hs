module Lib where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotPoints :: FilePath -> String -> Double -> [(Double, Double)] -> IO ()
plotPoints file title rad xs = toFile def file $ do
        layout_title .= title
        plot (pointsConfig rad <$> points "points" xs)

pointsConfig :: Double -> PlotPoints x y -> PlotPoints x y 
pointsConfig rad
    = set (plot_points_style.point_color) (opaque black)
    . set (plot_points_style.point_radius) rad
