module WeightReader (getWeights) where

rowWithWeight :: Double -> String -> [Double]
rowWithWeight weight charList = foldl (\ acc e -> (weight):acc) [] charList


readWeights :: String -> [(Double,String)]
readWeights s = read s::[(Double,String)]

getWeights :: String -> [Double]
getWeights s= buildWeights (readWeights s)

buildWeights :: [(Double,String)] -> [Double]
buildWeights ((a,bs):xs) = (rowWithWeight a bs)++(buildWeights xs)
buildWeights [] = []
