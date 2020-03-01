module PeakDetection (
    leftSignedDistances,
    rightSignedDistances,
    peakFunction,
    palshPeakDetection,
    signValues,
    peakDistances,
    averagePeakDistance
) where

import Util


leftSignedDistances :: (Real a) => Int -> Int -> [a] -> [a]
leftSignedDistances k i xs
    | k == 0 = []
    | (i - k) < 0 = leftSignedDistances (k - 1) i xs 
    | otherwise = (xs !! (i - k)) : leftSignedDistances (k - 1) i xs 

rightSignedDistances :: (Real a) => Int -> Int -> [a] -> [a]
rightSignedDistances k i xs
    | k == 0 = []
    | (i + k) >= length xs = rightSignedDistances (k - 1) i xs 
    | otherwise = (xs !! (i + k)) : rightSignedDistances (k - 1) i xs 

signValues :: (Real a) => [a] -> a -> [a]
signValues xs xi = map (\x ->  xi - x) xs

peakFunction :: (RealFrac a) => Int -> Int -> [a] -> a
peakFunction k i xs
    | k < i && i < (length xs - k) = (leftMax + rightMax) / 2
    | otherwise = 0
    where leftMax = maximum (signValues (leftSignedDistances k i xs) (xs !! i))
          rightMax = maximum (signValues (rightSignedDistances k i xs) (xs !! i))

isLargePeak :: (Ord a, Real a) => a -> a -> a -> Int -> Bool
isLargePeak x mean sd h = x > 0 && (x - mean) > (fromIntegral h * sd)

filterSmallPeaks :: (Real a, Ord a) => [(Int, a)] -> [a] -> a -> a -> Int -> [Int]
filterSmallPeaks a xs mean sd h = map (\(ix, x) -> ix) largePeaks
    where largePeaks = filter (\(ix, x) -> isLargePeak x mean sd h) a

palshPeakDetection :: (RealFrac a, Floating a) => [a] -> Int -> Int -> [Int]
palshPeakDetection xs k h = filterSmallPeaks zippedPeakFunctionValues xs meanOfPeakFunctionValues standardDeviationOfPeakFunctionValues h
    where zippedSequence = zip [0..] xs 
          peakFunctionValues = map (\(ix, x) -> peakFunction k ix xs) zippedSequence
          zippedPeakFunctionValues = zip [0..] peakFunctionValues
          positivePeakFunctionValues = filter (>= 0) peakFunctionValues
          meanOfPeakFunctionValues = mean positivePeakFunctionValues
          standardDeviationOfPeakFunctionValues = standardDeviation positivePeakFunctionValues meanOfPeakFunctionValues
        
peakDistances :: (Num a) => [a] -> [a]
peakDistances (x:xs)
    | length xs == 0 = []
    | otherwise = abs (x - (head xs)) : peakDistances xs

averagePeakDistance :: (Fractional b) => [Int] -> b 
averagePeakDistance xs = mean fracDistances
    where distances = peakDistances xs
          fracDistances = map (\x -> fromIntegral x) distances