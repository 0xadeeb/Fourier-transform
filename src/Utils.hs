module Utils where

import Data.Complex
import Text.Read (readMaybe)

readIntList :: String -> Maybe [Complex Double]
readIntList input = map (\x -> fromIntegral x :+ 0) <$> traverse readMaybe (words input)

padArray :: [Complex Double] -> Int -> [Complex Double]
padArray arr size = arr ++ replicate padSize 0
  where
    arrSize = length arr
    padSize = 2 ^ (ceiling (logBase 2 (fromIntegral size)) :: Int) - arrSize

selectElements :: [Complex Double] -> [Complex Double]
selectElements [] = []
selectElements [x] = [x]
selectElements arr = head arr : selectElements (tail $ tail arr)

