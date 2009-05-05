import Random
import System.IO.Unsafe (unsafePerformIO)
import Ratio
aRandom :: IO (Int)
aRandom = getStdRandom $ randomR (1,10)




randomList :: Int -> [IO (Int)]
randomList size = aRandom:(randomList (size -1))

rBool :: StdGen -> [Bool]
rBool gen = randoms gen::[Bool]

main :: IO ()
main = do myRandom <-  aRandom
          print myRandom
          --sequence randomList
	  gen <- newStdGen 
	  let ns = randoms gen::[Int]
	  let rs = randomRs (1,10) gen::[Int]
	  print $ take 10 ns
	  print $ take 10 rs
	  print $ take 10 (rBool gen)
	  let x = randomR (1,10) gen::(Integer,StdGen)
	  print $ fst x

	  let z =  next gen
	  print z
	  --print (myRandomList)
	  let g = truncate ((1%2) * 51)
	  print g
	  return ()


