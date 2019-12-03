module NeurNetwork where

import IzhNeuron
import qualified Data.Map as Map

data NeurNetwork = NeurNetwork {
                    neurons::Map.Map Int IzhNeuron,
                    synapses::Map.Map Int (Map.Map Int Float),
                    currents::Map.Map Int Float, 
                    fired::[Int]
                    } deriving (Show)

numNeurons :: NeurNetwork -> Int
numNeurons network = Map.size $ neurons network