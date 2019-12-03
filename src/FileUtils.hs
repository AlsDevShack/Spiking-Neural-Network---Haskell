module FileUtils where

import IzhNeuron
import Data.List 
import qualified Data.List.Split as T

saveSingleNeuronSim :: String -> [(IzhNeuron, Bool)] -> IO()
saveSingleNeuronSim fileName simulation = mapM_ (appendFile fileName) (intersperse   "\n" neuronStateStrings)
    where 
        neuronStateStrings = map (\(n, b) -> toFileString n) simulation

getState :: IzhNeuron -> (Float, Float)
getState (IzhNeuron params (IzhState v u)) = (v, u)

toFileString :: IzhNeuron -> String
toFileString neuron = (show v) ++"," ++ (show u)
    where 
        (v,u) = getState neuron

extractVoltages :: String -> [Float]
extractVoltages readInFile = map (\(v1,v2) -> v1) $ extractVals readInFile 

extractResetVals :: String -> [Float]
extractResetVals readInFile = map (\(v1,v2) -> v2) $ extractVals readInFile 

extractVals::String -> [(Float, Float)]
extractVals readInFile = map (\(v1,v2) -> (read v1, read v2)) valStrings
    where
        fileLines = T.splitOn "\n" readInFile 
        splitUpLines = map (T.splitOn ",") fileLines
        valStrings = map (\(v:v2) ->(v, head v2)) splitUpLines