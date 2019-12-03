module IzhNeuron (
    stepNeuron,
    IzhNeuron(..),
    IzhState(..),
    IzhParams(..)
) where 

import RungeKuttaSolver

data IzhParams = IzhParams {aParam::Float, bParam::Float, cParam::Float, dParam::Float} deriving (Show)

data IzhState = IzhState {vState::Float, uState::Float} deriving (Show)

data IzhNeuron = IzhNeuron {params::IzhParams, state::IzhState} deriving (Show)

dvdt::Float -> Float -> Float -> Float
dvdt u i v = 0.04*v^2 + 5*v + 140 - u +i

dudt:: Float -> Float -> Float -> Float -> Float
dudt a b v u = a*(b*v-u)

rungeKuttaDuDt :: Float -> Float -> Float -> Float -> Float -> Float
rungeKuttaDuDt timeStep a b prevV prevU = rungeKutta timeStep (dudt a b prevV) prevU

rungeKuttaDvDt :: Float -> Float -> Float -> Float -> Float
rungeKuttaDvDt  timeStep prevV prevU currCurrent = rungeKutta timeStep (dvdt prevU currCurrent) prevV

stepNeuron :: Float -> Float -> IzhNeuron -> (IzhNeuron, Bool)
stepNeuron timeStep current (IzhNeuron params@(IzhParams valA valB valC valD) (IzhState valV valU))
    | nextV >= 30 = (IzhNeuron params $ IzhState valC (nextU+valD), True)
    | otherwise =  (IzhNeuron params $ IzhState nextV nextU, False)
    where nextV = rungeKuttaDvDt timeStep valV valU current
          nextU = rungeKuttaDuDt timeStep valA valB valV valU

toString :: IzhNeuron -> String
toString (IzhNeuron (IzhParams a b c d) (IzhState v u)) = "v:" ++ (show v) ++ "; u:" ++ (show u) ++ "(a:" ++ (show a) ++"; b:" ++ (show b) ++ "; c:" ++ (show c) ++ ", d:" ++ (show d) ++")"