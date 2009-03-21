module ShuffleImperfect (shuffle) where

import Data.List (sortBy)
import Random

shuffle :: StdGen -> [a] -> [a]
shuffle gen as = sortBy (randomly gen) as
    where randomly gen  a b
           | (randBool gen) = GT
           | otherwise    = LT

randBool :: StdGen -> Bool
randBool gen = random gen::Bool



