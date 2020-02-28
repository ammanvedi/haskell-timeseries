import PeakDetection (leftSignedDistances, rightSignedDistances, signValues, peakFunction)
import Test.HUnit

testAutoCorrelationOutput = [1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544,1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544,1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544,1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544,1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544,1.0,0.2556962025316455,-0.3999999999999994,-0.3999999999999994,-0.3999999999999995,-0.3999999999999995,0.3443037974683544]

testLeftSigned = TestCase $ assertEqual
    "Should return correct leftSignedDistances" [0.2556962025316455,-0.3999999999999994,-0.3999999999999994] (leftSignedDistances 3 4 testAutoCorrelationOutput)

testLeftSignedFirstItem = TestCase $ assertEqual
    "Should return empty list for first item" [] (leftSignedDistances 3 0 testAutoCorrelationOutput)

testLeftSignedOverflow = TestCase $ assertEqual
    "Should return correct leftSignedDistances when overflows arr" [1] (leftSignedDistances 3 1 testAutoCorrelationOutput)

testRightSigned = TestCase $ assertEqual
    "Should return correct rightSignedDistances" [1.0,0.3443037974683544,-0.3999999999999995] (rightSignedDistances 3 4 testAutoCorrelationOutput)

testRightSignedOverlfow = TestCase $ assertEqual
    "Should return correct rightSignedDistances when overlfows arr" [0.3443037974683544,-0.3999999999999995] (rightSignedDistances 5 (length testAutoCorrelationOutput - 3) testAutoCorrelationOutput)

testRightSignedLastItem = TestCase $ assertEqual
    "Should return empty list for last item" [] (rightSignedDistances 5 (length testAutoCorrelationOutput - 1) testAutoCorrelationOutput)


testSignValues = TestCase $ assertEqual
    "Should correctly take values from items in array" [-2, -3, -4] (signValues [5, 6, 7] 3)

testPeakFunction = TestCase $ assertEqual
    "Should calculate correct S1 peak function for a value and k" (-0.33) (peakFunction 3 2 [1.0, 0.26, -0.4, -0.4, -0.4, -0.4])

main = do
    let tests = TestList 
            [ TestLabel "Left Signed Distance" testLeftSigned
            , TestLabel "Left Signed Distance Overflow" testLeftSignedOverflow
            , TestLabel "Left Signed Distance First Item" testLeftSignedFirstItem
            , TestLabel "Right Signed Distance" testRightSigned
            , TestLabel "Right Signed Distance Overflow" testRightSignedOverlfow
            , TestLabel "Right Signed Distance Last Item" testRightSignedLastItem
            , TestLabel "Sign Values" testSignValues 
            , TestLabel "Peak Function" testPeakFunction
            ]
    runTestTT tests