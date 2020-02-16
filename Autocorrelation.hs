
module Autocorrelation (
  autoCorrelation
) where

mean :: (Fractional a) => [a] -> a
mean xs = summed / fromIntegral (length xs)
  where summed = sum xs


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

divTuple :: (Fractional a) => (a, a) -> a
divTuple (x, y) = x / y

autoCorrelationAtIndex :: (Fractional a) => [a] -> Int -> a
autoCorrelationAtIndex xs index = divTuple summedTuple
  where meanVal = mean xs
        fractionsToSum = getFractionComponentsToSum meanVal xs (length xs - 1) index
        summedTuple = sumTupleComponents fractionsToSum

autoCorrelation :: (Fractional a) => [a] -> [a]
autoCorrelation xs = [autoCorrelationAtIndex xs i | (i, x) <- zip [0..] xs ]