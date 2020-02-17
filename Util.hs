module Util ( mean, standardDeviation  ) where
    

mean :: (Fractional a) => [a] -> a
mean xs = summed / fromIntegral (length xs)
    where summed = sum xs

standardDeviation :: (Floating a) => [a] -> a -> a
standardDeviation xs meanVal = sqrt meanSquaredDistances
    where squaredDifferences = map (\x -> (x - meanVal) * (x - meanVal) ) xs
          meanSquaredDistances = mean squaredDifferences