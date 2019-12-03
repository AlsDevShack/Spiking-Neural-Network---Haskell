module NetworkGenerator where 

import NeurNetwork
import NetworkBuilder
import SingleNeuronSim
import Helpers
import RandomTools
import System.Random 
import qualified Data.Map as Map

genNetwork :: Int -> (Int, Int) -> StdGen -> NeurNetwork
genNetwork n range randomiser = genSynapses unConnected range randomiser
    where 
        unConnected = genNeurons n 

genNeurons :: Int -> NeurNetwork
genNeurons n = foldl (\network key -> addNeuron network key standardNeuron) newNetwork [1..n]

genSynapses :: NeurNetwork -> (Int,Int) -> StdGen -> NeurNetwork
genSynapses network numSynapsesPerNeuron randGen = createSynapses network synapses 
    where 
        synapses = genRandSynapses randGen numN (1, numN) numSynapsesPerNeuron
        numN = numNeurons network

createSynapses:: NeurNetwork -> [[(Int,Float)]] -> NeurNetwork
createSynapses network generatedSynapses = foldl (\network (n, syns) -> createSynapsesForNeuron network n syns) network synapsesPerNeuron
    where
        synapsesPerNeuron = zip keys generatedSynapses
        keys = Map.keys $ neurons network

createSynapsesForNeuron :: NeurNetwork -> Int -> [(Int, Float)] -> NeurNetwork
createSynapsesForNeuron network neuronKey generatedSynapses = foldl (\network synapse -> addSynapse network neuronKey synapse) network generatedSynapses

genRandSynapses:: StdGen -> Int -> (Int, Int) -> (Int,Int) -> [[(Int,Float)]]
genRandSynapses randGen n synapseKeyRange numSynapsesPerGroup = chunks numSynapsesPerNeuron $ genRandSynapseList randGen1 synapseKeyRange
    where 
        numSynapsesPerNeuron = take n $ randomRs numSynapsesPerGroup randGen2
        randGen1 = nextGen randGen
        randGen2 = nextGen randGen1

genRandSynapseList:: StdGen -> (Int, Int) -> [(Int, Float)]
genRandSynapseList randGen range = zip synapsesTo weights
    where
        synapsesTo = randomRs range randGen1
        weights = randomRs (0.0, 1.0) randGen2
        randGen1 = nextGen randGen
        randGen2 = nextGen randGen1