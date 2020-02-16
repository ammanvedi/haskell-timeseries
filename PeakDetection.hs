module PeakDetection (
    leftSignedDistances,
    rightSignedDistances,
    peakFunction,
) where

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