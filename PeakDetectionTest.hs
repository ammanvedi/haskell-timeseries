import PeakDetection (leftSignedDistances, rightSignedDistances, signValues)
import Test.HUnit

testLeftSigned = TestCase $ assertEqual
    "Should return correct leftSignedDistances" [2, 3, 4] (leftSignedDistances 3 4 [1, 2, 3, 4, 5, 6, 7, 8])

testLeftSignedOverflow = TestCase $ assertEqual
    "Should return correct leftSignedDistances when overflows arr" [1] (leftSignedDistances 3 1 [1, 2, 3, 4, 5, 6, 7, 8])

testRightSigned = TestCase $ assertEqual
    "Should return correct rightSignedDistances" [8, 7, 6] (rightSignedDistances 3 4 [1, 2, 3, 4, 5, 6, 7, 8])

testRightSignedOverlfow = TestCase $ assertEqual
    "Should return correct rightSignedDistances when overlfows arr" [8, 7, 6] (rightSignedDistances 5 4 [1, 2, 3, 4, 5, 6, 7, 8])

testSignValues = TestCase $ assertEqual
    "Should correctly take values from items in array" [-2, -3, -4] (signValues [5, 6, 7] 3)

main = do
    let tests = TestList 
            [ TestLabel "lsd" testLeftSigned
            , TestLabel "lsdOverflow" testLeftSignedOverflow
            , TestLabel "rsd" testRightSigned
            , TestLabel "rsdOverflow" testRightSignedOverlfow
            , TestLabel "signed" testSignValues 
            ]
    runTestTT tests