module Plotter where

import Graphics.EasyPlot

plotList :: [Float] -> IO Bool
plotList list = plot' [Interactive] Windows (convertList list)

convertList :: [Float] -> Graph2D Float Float
convertList list = Data2D [Style Lines] [] (zip [1..] list)