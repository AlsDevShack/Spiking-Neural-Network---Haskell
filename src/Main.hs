module Main where

import Control.Monad 
import SingleNeuronSim 
import FileUtils
import Plotter

timeStep = 0.1
a = 0.02 
b = 0.2
c = (-65)
d = 8

fileName = "sim.txt"

main = do
    let simulated = simulateNeuron timeStep 2000 15 $ makeNeuron a b c d 
    saveSingleNeuronSim fileName simulated
    readVoltages


readVoltages :: IO ()
readVoltages = do
    contents <- readFile fileName
    plotList $ extractVoltages contents
    return ()
