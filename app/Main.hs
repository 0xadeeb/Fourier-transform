module Main (main) where

import FourierTransform
import Data.Complex
import System.Directory.Internal.Prelude (exitFailure)
import System.IO
import Utils

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter coeff of the polinomial1 in order (p0, p1, p2..., pd): "
  input1 <- getLine
  putStr "Enter coeff of the polinomial2 in order (p0, p1, p2..., pd): "
  input2 <- getLine
  case (readIntList input1, readIntList input2) of
    (Just p1, Just p2) -> do
      let prod_len = length p1 + length p2 - 1
      let v1 = rFFT $ padArray p1 prod_len
      let v2 = rFFT $ padArray p2 prod_len
      putStr "Product of the two polinomials are: "
      mapM_ ((\p -> print p >> putChar ' ') . round . realPart) (take prod_len $ iFFT [x * y | (x, y) <- zip v1 v2])

    _ -> do
      putStrLn "Invalid input. Please enter a valid list of integers."
      exitFailure
