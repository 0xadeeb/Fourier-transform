module FourierTransform where

import Data.Complex
import Utils

baseFFT :: Double -> [Complex Double] -> [Complex Double]
baseFFT _ [p0] = [p0]
baseFFT phase p = [ve_j + ω ^ (j `mod` (n `div` 2)) * vo_j | (ve_j, vo_j, j) <- zip3 (ve ++ ve) (vo ++ map negate vo) [0 .. (n - 1)]]
  where
    n = length p
    ω = cis (2 * pi * phase / fromIntegral n)
    ve = baseFFT phase $ selectElements p
    vo = baseFFT phase $ selectElements $ tail p

rFFT :: [Complex Double] -> [Complex Double]
rFFT = baseFFT 1

iFFT :: [Complex Double] -> [Complex Double]
iFFT p = map (/ fromIntegral n) $ baseFFT (-1) p
  where
    n = length p

