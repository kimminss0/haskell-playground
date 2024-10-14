module Main where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Data.Word (Word64)

main :: IO ()
main = do
  args <- getArgs
  let num = readMaybe =<< listToMaybe args
  case num of
    Just num' -> print $ fib num'
    Nothing -> exitFailure

fib :: Int -> Word64
fib n = fib' n 0 1
  where
    fib' :: Int -> Word64 -> Word64 -> Word64
    fib' 0 !a !_ = a
    fib' !k !a !b = fib' (k - 1) (b `mod` m) ((a + b) `mod` m)
    m = 18446744073709551557 :: Word64

