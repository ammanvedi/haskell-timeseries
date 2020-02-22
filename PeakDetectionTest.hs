import PeakDetection (leftSignedDistances)
import Test.HUnit

testLeftSigned = TestCase $ assertEqual
    "Should return correct leftSignedDistances" [2, 3, 5] (leftSignedDistances 3 4 [1, 2, 3, 4, 5, 6, 7, 8])

main = runTestTT testLeftSigned