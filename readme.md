# Haskell Time Series Utils

This repo contains some utils for analysis of time series, its a work in progress at the moment. Its a project I am using as i learn more haskell.



## Utils

#### Autocorrelation

Autocorrelation provides the cross correlation of a signal with itself, it is useful for identifying seasonality/repeating patterns.

```haskell
autoCorrelation :: (Fractional a) => [a] -> [a]
```

#### Peak Detection

Peak detection is based on [Simple Algorithms for Peak Detection in Time-Series](https://pdfs.semanticscholar.org/1d60/4572ec6ed77bd07fbb4e9fc32ab5271adedb.pdf) by Girish Keshav Palshikar. Pseudo code can be found in the linked paper.

```haskell
-- Gives the indexes in input array of peaks
palshPeakDetection :: (RealFrac a, Floating a) => [a] -> Int -> Int -> [Int]

-- Gives the average distance between said indexes
averagePeakDistance :: (Fractional b) => [Int] -> b 
```
