module SingleNeuronSim where

import IzhNeuron

makeNeuron::Float -> Float -> Float -> Float -> IzhNeuron
makeNeuron a b c d = IzhNeuron (IzhParams a b c d ) (IzhState c 0)

standardNeuron:: IzhNeuron
standardNeuron = makeNeuron 0.02 0.2 (-65) 8

simulateNeuron :: Float -> Int -> Float -> IzhNeuron -> [(IzhNeuron, Bool)]
simulateNeuron timeStep steps current neuron =  reverse $ foldl stepNeuronList ((neuron, False) : []) [1..steps]
    where
        stepNeuronList list _ = (stepNeuron timeStep current $ fst $ head list) : list

