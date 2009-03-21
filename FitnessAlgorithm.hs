module FitnessAlgorithm (distanceFitnessAlgorithm,getFitnessAlgorithm,FitnessAlgorithm) where

import Data.List(sum,find,(!!))
import Keyboard(Keyboard,getKeyPos)
import Data.Maybe(fromJust)

data FitnessAlgorithm = FitnessAlgorithm {
 input :: [String], 
 letterWeight :: [Double]
}deriving(Show)

distanceFitnessAlgorithm :: Keyboard -> FitnessAlgorithm -> Double
distanceFitnessAlgorithm k fa = sum weightList
              where weightList = (weights (input fa) k (letterWeight fa))


--for every element in string determine it's position on keyboard's keys, get the weight from the weight list, add it to a list
weights :: [String] -> Keyboard -> [Double] -> [Double]
weights (c:cs) k ws = ((findWeight k ws c)):(weights cs k ws)
weights [] k ws = []


findWeight :: Keyboard -> [Double] -> String -> Double
findWeight k ws c = ws !! (saftelyRetrieve (getKeyPos  c k) c)
      where saftelyRetrieve Nothing c = error $"could not find weight for item:"++ c
            saftelyRetrieve (Just weight) c = weight


getFitnessAlgorithm :: [String] ->[Double] -> FitnessAlgorithm
getFitnessAlgorithm a b = FitnessAlgorithm a b
