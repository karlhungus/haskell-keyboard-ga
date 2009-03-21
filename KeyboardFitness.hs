module KeyboardFitness 
           (calculateFitness,
            truncationSelect,
	    twoPointCrossOver,
	    breedPopulation,
	    runGenerations,
	    runGeneration) where

import Keyboard(Keyboard,breedKeyboards,kLength)
import FitnessAlgorithm(distanceFitnessAlgorithm,FitnessAlgorithm)
import Data.List(take,drop,sortBy)
import Data.Ratio((%))
import Random(StdGen,randomR)

calculateFitness :: [Keyboard] -> FitnessAlgorithm -> [(Double,Keyboard)]
calculateFitness (k:ks) fa =  ((distanceFitnessAlgorithm k fa),k):(calculateFitness ks fa)
calculateFitness [] fa = []



--Selection Techniques 
--
--Truncation Selection
truncationSelect :: [(Double,Keyboard)] -> Rational -> [(Double,Keyboard)]
truncationSelect f a = take (numToTake (length f) a) (sorted f)
                      where sorted fks = sortBy (\ (f1,_) (f2,_) -> compare f1 f2) fks 
                            numToTake l a= (truncate ((toRational l) * a)::Int)



--Roulette Selection
selectionProbability :: [(Double,Keyboard)] -> Double -> [(Double,Keyboard)]
selectionProbability [] fitnessSum = []
selectionProbability ((f,k):fks) fitnessSum = ((prob f),k):(selectionProbability fks fitnessSum)
                                                where prob f  = f / fitnessSum 




--Crossover Techniques -- p1 and p2 are less then keyboard length
twoPointCrossOver :: (Keyboard,Keyboard) -> Int->Int -> Keyboard
twoPointCrossOver (k1,k2) pt1 pt2 = breedKeyboards (k1,k2) pt1 pt2

runGenerations :: [Keyboard] -> FitnessAlgorithm -> StdGen -> Int -> [Keyboard]
runGenerations ks fa gen 0 = ks
runGenerations ks fa gen size = runGenerations (runGeneration ks fa gen) fa gen (size - 1)

runGeneration :: [Keyboard] -> FitnessAlgorithm -> StdGen -> [Keyboard]
runGeneration ks fa gen = breedPopulation (boardsOnly (truncCalcFit ks fa)) gen
                            where truncCalcFit ks fa = (truncationSelect (calculateFitness ks fa) (1%2))
	                          boardsOnly fks = foldl (\ acc (f,k) -> k:acc) [] fks 

--Population mateing
breedPopulation :: [Keyboard] -> StdGen -> [Keyboard]
breedPopulation ks gen = ks ++ (breedPairs (rSelectPairs ks (length ks) gen)) gen

rSelectPairs :: [Keyboard] -> Int -> StdGen -> [(Keyboard,Keyboard)]
rSelectPairs k 0 gen = []
rSelectPairs k numPairs gen = ( (fst (selectRandomKeyboard k gen)), (fst (selectRandomKeyboard k (snd (selectRandomKeyboard k gen)))) ):(rSelectPairs k (numPairs -1) (snd (nextRand (length k) gen)))

selectRandomKeyboard :: [Keyboard] -> StdGen -> (Keyboard,StdGen)
selectRandomKeyboard k gen = ( (k !! (fst (nextRand (length k -1) gen))) , (snd (nextRand (length k - 1 )  gen)) )

breedPairs :: [(Keyboard,Keyboard)] -> StdGen -> [Keyboard]
breedPairs [] gen = []
breedPairs ((k1,k2):ks) gen = (twoPointCrossOver (k1,k2) localNextRand2 localNextRand ):(breedPairs ks (snd (nextRand (kLength k1) nextGen2)))
                               where localNextPair = (nextRand (kLength k1) gen)
			             nextGen = snd localNextPair
				     localNextRand = fst localNextPair
				     localNextPair2 = (nextRand (localNextRand) nextGen)
				     localNextRand2 = fst localNextPair2
				     nextGen2 = snd localNextPair2

nextRand :: Int -> StdGen -> (Int,StdGen)
nextRand l g = randomR (0,l) g::(Int,StdGen)
