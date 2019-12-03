module RandomTools where

import System.Random 

nextGen :: StdGen -> StdGen
nextGen gen = snd  ((random gen)::(Int, StdGen))
