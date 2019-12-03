module NetworkBuilder where 

import IzhNeuron
import NeurNetwork
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

newNetwork::NeurNetwork
newNetwork = NeurNetwork Map.empty Map.empty Map.empty []

addNeuron:: NeurNetwork -> Int -> IzhNeuron -> NeurNetwork
addNeuron (NeurNetwork neurons synapses currents fired) key neuron = NeurNetwork updatedNeurons updatedSynapses updatedCurrents fired
    where 
        updatedNeurons = Map.insert key neuron neurons
        updatedSynapses = Map.insert key Map.empty synapses
        updatedCurrents = Map.insert key 0.0 currents

addSynapse::NeurNetwork -> Int -> (Int, Float) -> NeurNetwork
addSynapse (NeurNetwork neurons synapses currents fired) from (to,weight) = NeurNetwork neurons updatedSynapses currents fired
    where 
        updatedSynapses = addSynapse' synapses from to weight

addSynapse' :: Map.Map Int (Map.Map Int Float) -> Int -> Int -> Float -> Map.Map Int (Map.Map Int Float)
addSynapse' synapses from to weight 
    | not $ Map.member from synapses = synapses
    | not $ Map.member to synapses = synapses
    | otherwise = Map.insert from updatedSynapseConnections synapses
        where updatedSynapseConnections = Map.insert to weight $ Maybe.fromJust (Map.lookup from synapses)

addCurrent :: NeurNetwork -> Int -> Float -> NeurNetwork
addCurrent (NeurNetwork neurons synapses currents firingNeurons) neurKey current = (NeurNetwork neurons synapses updatedCurrents firingNeurons)
    where updatedCurrents = addCurrent' currents neurKey current

addCurrent' :: Map.Map Int Float -> Int -> Float -> Map.Map Int Float
addCurrent' currents neurKey current 
    | not $ Map.member neurKey currents = currents
    | otherwise = Map.insert neurKey (Maybe.fromJust (Map.lookup neurKey currents) + current) currents