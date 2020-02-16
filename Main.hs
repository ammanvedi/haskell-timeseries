module Main where

import Autocorrelation
import PeakDetection

main :: IO ()
main = do
  let nums = [0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1 ]
  let ac = autoCorrelation nums
  let pf = peakFunction 3 4 [1, 2, 3, 4, 5, 6, 7, 8, 9]
  print pf