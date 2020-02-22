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

peakFunction :: (RealFrac a) => Int -> Int -> [a] -> a
peakFunction k i xs = (leftMax + rightMax) / 2
    where leftMax = maximum (leftSignedDistances k i xs)
          rightMax = maximum (rightSignedDistances k i xs)

isLargePeak :: (Ord a, Real a) => a -> a -> a -> Int -> Bool
isLargePeak x mean sd h = x > 0 && (x - mean) > (fromIntegral h * sd)

filterSmallPeaks :: (Real a, Ord a) => [(Int, a)] -> [a] -> a -> a -> Int -> [a]
filterSmallPeaks a xs mean sd h = map (\(ix, x) -> (xs !! ix)) largePeaks
    where largePeaks = filter (\(ix, x) -> isLargePeak x mean sd h) a

palshPeakDetection :: (RealFrac a, Floating a) => [a] -> Int -> Int -> [a]
palshPeakDetection xs k h = filterSmallPeaks zippedPeakFunctionValues xs meanOfPeakFunctionValues standardDeviationOfPeakFunctionValues h
    where zippedSequence = zip [0..] xs 
          peakFunctionValues = map (\(ix, x) -> peakFunction k ix xs) zippedSequence
          zippedPeakFunctionValues = zip [0..] peakFunctionValues
          positivePeakFunctionValues = filter (>= 0) peakFunctionValues
          meanOfPeakFunctionValues = mean positivePeakFunctionValues
          standardDeviationOfPeakFunctionValues = standardDeviation positivePeakFunctionValues meanOfPeakFunctionValues