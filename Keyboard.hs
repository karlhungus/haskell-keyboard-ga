module Keyboard (
            Keyboard,
	    getKeyboard,
	    getKeyPos,
	    breedKeyboards,
	    kLength,
	    name) where 

import Data.List(elemIndex,filter,any,splitAt)

type KeyCmd = String

data Key = Key {
        keyCmd ::KeyCmd 
}	deriving (Show,Read,Eq)
data Keyboard = Keyboard {
	keys :: [Key],
	name :: String
} deriving (Show,Read)


getKeys :: [String] -> [Key]
getKeys [] = []
getKeys (x:xs) = Key x :(getKeys xs)

getKeyboard :: [String] -> Keyboard
getKeyboard (q:w:e:r:t:y:rest) = Keyboard (getKeys (q:w:e:r:t:y:rest)) (q++w++e++r++t++y) --(q++w++e++r++t++y++[])

fromKeys :: [Key] -> Keyboard
fromKeys keys = Keyboard keys (foldl (\ acc a -> acc++(keyCmd a)) [] (take 6 keys))

getKeyPos :: String -> Keyboard -> Maybe Int
getKeyPos c k = elemIndex (Key c)  (keys k)

kLength :: Keyboard -> Int
kLength k = length (keys k)

breedKeyboards :: (Keyboard, Keyboard)-> Int -> Int -> Keyboard
breedKeyboards (p1,p2) pt1 pt2 
               | pt1 < pt2 = removeAndShoveAt p1 (getSection (keys p2) pt1 pt2) pt1
               | otherwise = removeAndShoveAt p1 (getSection (keys p2) pt2 pt1) pt2
	       where getSection ks start end = (fst (splitAt (end - start) (snd (splitAt start (ks)))))
--fromKeys ((take pt (keys p1)) ++ (drop pt (keys p2)))


removeAndShoveAt :: Keyboard -> [Key] -> Int -> Keyboard
removeAndShoveAt kb ks pos = fromKeys ((fst splitKeys) ++ ks ++ (snd splitKeys))
                              where splitKeys = (splitAt pos (removeKeys kb ks))

removeKeys :: Keyboard -> [Key] -> [Key]
removeKeys kb ks = (filter (\ a -> not (isInKeys a ks)) (keys kb))
                   where isInKeys a ks = any (\ item -> a == item) ks
--Choose X keys from keyboard 2 and move keyboard 1's verison of those keys to those positions
swapKeys ::Keyboard -> Keyboard -> Key -> Key -> Keyboard 
swapKeys kb1 kb2 k1 k2 = undefined


