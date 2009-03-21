import HUnit
import diNovoEdgeWeights
test1 = TestCase ( assertEqual "should have a string followed by a weight" (digitRow !! 11) (("+",4))


tests = TestList [TestLabel "test1" test1]


