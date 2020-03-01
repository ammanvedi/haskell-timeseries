module Main where

import Autocorrelation
import PeakDetection
import Util

main :: IO ()
main = do
  let nums = [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
  let ac = autoCorrelation nums
  let pd = palshPeakDetection ac 4 1
  let ad = averagePeakDistance pd
  print "Average Peak Distance"
  print ad