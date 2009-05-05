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
         let boards = generateKeyboards (fst (fst inputArgs)) gen
             printedBoards = show boards
             readBoards = read printedBoards::[Keyboard]

         --Input Data File
	 inputDF <- openFile (fst (snd inputArgs)) ReadMode
         inputDFS <-hGetContents inputDF
         
         --Input Weight File
	 inputWs <- openFile (snd (snd inputArgs)) ReadMode
	 inputWsS <- hGetContents inputWs

         --Only deal with a-z for now
	 let nothingbutVanella = filter (\ a -> any (\ x  -> x == a)  (['a'..'z'])) inputDFS
	 let charsToStrings = foldl (\ acc a -> (a:""):acc) [] nothingbutVanella
	 
	 let readWeights = getWeights inputWsS
         let fa = getFitnessAlgorithm (charsToStrings) readWeights

         --number of generations to read in
	 let nogenerations = (snd (fst inputArgs))
	 let banner =  "==========================Running Generations"++ (show nogenerations) ++"==============================="
	 print banner
	 let initFitness = calculateFitness readBoards fa
	 let generation = (runGenerations readBoards fa gen nogenerations)
	 let  postFitness = calculateFitness generation fa

         print "Init Gen"
	 print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] initFitness
	 print "Subsequent Gen"
	 print $ foldl (\ acc a -> (fst a,(name (snd a))):acc) [] postFitness

         return ()
