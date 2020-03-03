module SeasonalityMatcher (
    generateBinaryPermutationsOfLength
) where

import Numeric
import Data.Char

rightPad :: String -> Char -> Int -> String
rightPad str padChar len
    | length str >= len = str
    | otherwise = str ++ map (\_ -> padChar) [0..(len - length str)]

generateBinaryPermutationsOfLength :: Int -> [String]
generateBinaryPermutationsOfLength len
    | len > 15 = []
    | otherwise = map (\x -> toFixedLenBin x) [0..(2 ^ len - 1)]
        where toFixedLenBin val = rightPad (toBin val) '0' len
              toBin val = showIntAtBase 2 intToDigit val ""