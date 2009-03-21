module KeyboardFactory (generateKeyboards,generateKeyboard) where

import Data.List (genericIndex,sortBy)
import Shuffle (shuffle)
--import NaiveShuffle (shuffle)
--import ShuffleImperfect (shuffle)
import Random (newStdGen,randomRs,randomR,StdGen,next)
import Keyboard (getKeyboard,Keyboard)

--Constants 
atoz=foldl (\ acc c -> acc ++ [c:""]) [] ['a'..'z']
digits=foldl (\ acc d -> acc ++ [show d]) [] [0..9]
other=foldl (\ acc c -> acc ++ [c:""]) [] ['-','=','`','\'','\\','/',',','.','~']
fkeys=foldl (\ acc x -> acc++['F':(show x)]) [] [1..12]
meta=["lalt","lctrl","rctrl","ralt","rshift","lshift","capslock","tab","esc","scrllck","printScr","numlck","up","down","left","right","home","end","pgdwn","pgup","enter","win","ctx","insert","bkspace","delete"]


total :: Int
total = (length atoz) + (length digits)
--total=length atoz +  length digits + length other + length fkeys + length meta

--build all keys
getKeyList :: [String]
getKeyList = (atoz) ++ (digits)
--getKeyList = (atoz) ++ ( digits) ++ (other) ++ ( fkeys) ++ ( meta)

--make a randomized keyboard
randomKeyList :: [Integer] -> [String]
randomKeyList randomList = shuffle getKeyList randomList

--Make a random shuffle for a key list
randomKeyShuffle :: StdGen -> [Integer]
randomKeyShuffle gen = diminishingRandomList (total -1) gen


randomRange :: (Integer,Integer) -> StdGen -> [Integer] 
randomRange (x,y) gen = randomRs (x,y) gen::[Integer]

pairRandomR :: (Integer,Integer) -> StdGen -> (Integer,StdGen)
pairRandomR (x,y) gen = randomR (x,y) gen::(Integer,StdGen)

--Generate a shuffled list of random numbers of length size, and no greater then size -n at position n
diminishingRandomList :: Int -> StdGen -> [Integer]
diminishingRandomList size gen 
                    | size == 0 = []
		    | otherwise = (fst (nextPair size))  : (diminishingRandomList (size -1) (snd (nextPair size)))
                          where nextPair size =(pairRandomR ((toInteger 0),(toInteger (size -1))) gen)
generateKeyboards :: Int -> StdGen -> [Keyboard]
generateKeyboards size gen  
                    | size == 0 = []
                    | otherwise = (generateKeyboard gen):(generateKeyboards (size -1) (snd $next gen))

generateKeyboard :: StdGen -> Keyboard
generateKeyboard gen = getKeyboard (randomKeyList (randomKeyShuffle gen)) 
