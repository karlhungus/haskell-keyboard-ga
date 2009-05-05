import FitnessAlgorithm(distanceFitnessAlgorithm,getFitnessAlgorithm,FitnessAlgorithm)
import Test.QuickCheck

--instance Arbitrary Char where
-- arbitirary = choose ('\32','\128')
-- coarbitrary c = variant (ord c `rem` 4)

quickCheck getFitnessAlgorithm
