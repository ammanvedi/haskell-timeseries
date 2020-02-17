module PeakDetection (
    leftSignedDistances,
    rightSignedDistances,
    peakFunction,
    palshPeakDetection,
) where

import Util


leftSignedDistances :: (Real a) => Int -> Int -> [a] -> [a]
leftSignedDistances 0 _ _ = []
leftSignedDistances k i xs = (xs !! (i - k)) : leftSignedDistances (k - 1) i xs 

rightSignedDistances :: (Real a) => Int -> Int -> [a] -> [a]
rightSignedDistances 0 _ _ = []
rightSignedDistances k i xs = (xs !! (i + k)) : rightSignedDistances (k - 1) i xs 

peakFunction :: (Real a, Fractional b) => Int -> Int -> [a] -> b
peakFunction k i xs = realToFrac (leftMax + rightMax) / 2
    where leftMax = maximum (leftSignedDistances k i xs)
          rightMax = maximum (rightSignedDistances k i xs)



palshPeakDetection :: (Real a) => [a] -> Int -> Int -> [a]
palshPeakDetection xs k h = xs
    where zippedSequence = zip [0..] xs 
          peakFunctionValues = map (\(ix, x) -> peakFunction k ix xs) zippedSequence
          positivePeakFunctionValues = filter (\x -> x >= 0) peakFunctionValues
          meanOfFunctionValues = mean positivePeakFunctionValues
          standardDeviationOfFunctionValues = standardDeviation positivePeakFunctionValues meanOfFunctionValues