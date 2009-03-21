--module Runner (main) where

import System.Environment(getArgs)
import Random(newStdGen)
import KeyboardFactory(generateKeyboards)
import Keyboard(Keyboard,name)
import KeyboardFitness(calculateFitness,truncationSelect,twoPointCrossOver,breedPopulation,runGenerations,runGeneration) 


import FitnessAlgorithm(getFitnessAlgorithm)
import WeightReader(getWeights)
import System.IO
import System.Directory
import Ratio((%))


determineArgs :: [String] -> ((Int,Int),(String,String))
determineArgs (a:b:c:d:[]) = (((read a::Int),(read b::Int)),(c,d))
determineArgs _ = error "Useage: init population size, # generations, test data file, weight file,"

main :: IO ()
main = do
         args <- getArgs
         let inputArgs = determineArgs args 
	 
	 gen <- newStdGen
         let boards = generateKeyboards (fst (fst inputArgs)) gen--numberOfKeyboards gen
             printedBoards = show boards
             readBoards = read printedBoards::[Keyboard]
	 print "==================Read Boards=============================="
         --print $ foldl (\ acc a -> (name a):acc) [] boards
         
	 inputDF <- openFile (fst (snd inputArgs)) ReadMode
         inputDFS <-hGetContents inputDF
         
	 inputWs <- openFile (snd (snd inputArgs)) ReadMode
	 inputWsS <- hGetContents inputWs

	 print "==================Input Data File==========================="
	 print inputDFS
	 let nothingbutVanella = filter (\ a -> any (\ x  -> x == a)  (['a'..'z'])) inputDFS
	 let charsToStrings = foldl (\ acc a -> (a:""):acc) [] nothingbutVanella
	 
         print "==================Input Weights============================"
	 print inputWsS
         print "==================Read Weight File========================"
	 let readWeights = getWeights inputWsS
	 --print readWeights
         let fa = getFitnessAlgorithm (charsToStrings) readWeights
	 let fitnesses = calculateFitness readBoards fa 
	 --print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] fitnesses
	 
         let trunc = truncationSelect fitnesses (1%2)
	 --print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] trunc
	 --FitnessAlgorithm
	 let parents = (snd (trunc !! 0), snd (trunc !! 1))
         let child = twoPointCrossOver parents  3 7
	 
        -- print $ (name (fst parents),name (snd parents))
	-- print $ name child
	 print "============================BREED POP=============================="
	 let newPop = breedPopulation (foldl (\ acc a -> (snd a):acc) [] trunc) gen 
	 --print $ foldl (\ acc a -> (name a):acc) [] newPop

	 let nogenerations = (snd (fst inputArgs))
	 let banner =  "============================RUN GEN "++ (show nogenerations) ++"==============================="
	 print banner
	 let initFitness = calculateFitness readBoards fa
	 let generation = (runGenerations readBoards fa gen nogenerations)
	 --let generation = runGeneration readBoards fa gen 
	 let  postFitness = calculateFitness generation fa

         print "Init Gen"
	 print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] initFitness
	 print "Subsequent Gen"
	 print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] postFitness

         return ()
