
module Autocorrelation (
  autoCorrelation
) where

import Util


calcNumeratorDenominator :: (Fractional b) => b -> [b] -> Int -> Int -> (b,b)
calcNumeratorDenominator mean xs i t = (numerator, xim * xim)
  where xIndex = (i + t) `mod` (length xs)
        xim = (xs !! i) - mean
        numerator = xim * ((xs !! xIndex) - mean)

getFractionComponentsToSum :: (Fractional b) => b -> [b] -> Int -> Int -> [(b, b)]
getFractionComponentsToSum _ _ 0 _ = []
getFractionComponentsToSum mean xs i t = newItem : getFractionComponentsToSum mean xs (i - 1) t
  where newItem = calcNumeratorDenominator mean xs i t


sumTupleComponents :: (Fractional a) => [(a, a)] -> (a, a)
sumTupleComponents = foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)

divTuple :: (Double, Double) -> Double
divTuple (x, y) = x / y

autoCorrelationAtIndex :: [Double] -> Int -> Double
autoCorrelationAtIndex xs index = divTuple summedTuple
  where meanVal = mean xs
        fractionsToSum = getFractionComponentsToSum meanVal xs (length xs - 1) index
        summedTuple = sumTupleComponents fractionsToSum

autoCorrelation :: [Double] -> [Double]
autoCorrelation xs = [autoCorrelationAtIndex xs i | (i, x) <- zip [0..] xs ]