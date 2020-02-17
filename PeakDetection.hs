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

isSmallPeak :: (Fractional a, Floating b) => a -> a -> b -> Int -> Bool
isSmallPeak x mean sd h = x > 0 && (x - mean) > (h * sd)

filterSmallPeaks :: (Fractional a, Floating b, Real c) => [(Int, a)] -> [c] -> a -> b -> Int -> [a]
filterSmallPeaks a xs mean sd h = map (\(ix, x) -> xs !! ix) a
    where largePeaks = filter (\(ix, x) -> isSmallPeak x mean sd h) a

palshPeakDetection :: (Real a) => [a] -> Int -> Int -> [a]
palshPeakDetection xs k h = filterSmallPeaks zippedPeakFunctionValues xs meanOfPeakFunctionValues standardDeviationOfPeakFunctionValues h
    where zippedSequence = zip [0..] xs 
          peakFunctionValues = map (\(ix, x) -> peakFunction k ix xs) zippedSequence
          zippedPeakFunctionValues = zip [0..] peakFunctionValues
          positivePeakFunctionValues = filter (>= 0) peakFunctionValues
          meanOfPeakFunctionValues = mean positivePeakFunctionValues
          standardDeviationOfPeakFunctionValues = standardDeviation positivePeakFunctionValues meanOfPeakFunctionValues