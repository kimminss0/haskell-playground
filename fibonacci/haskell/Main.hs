module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (num, fib) -> print $ fib num
    Nothing -> exitFailure
  where
    parseArgs :: [String] -> Maybe (Int, Int -> Int)
    parseArgs [a1, a2] = do
      num <- readMaybe a1
      sel <- readMaybe a2
      fib <- fibWith sel
      Just (num, fib)
    parseArgs _ = Nothing

    fibWith :: Int -> Maybe (Int -> Int)
    fibWith 1 = Just fib1
    fibWith 2 = Just fib2
    fibWith 3 = Just fib3
    fibWith _ = Nothing

-- Elegant example demonstrating the nature of Haskell's lazy evaluation.
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- Inefficient due to linear space complexity. Since we only need a single term
-- of the Fibonacci sequence, this approach creates numerous intermediate
-- values which cannot be garbage collected.
fib1 :: Int -> Int
fib1 n = fibs !! n

-- Lazy-evaluated function arguments lead to poor performance, as the thunk, an
-- unevaluated expression that can be represented as a deep tree, grows and
-- becomes increasingly complex, allocating a lot of memory in the heap, which
-- may result in many cache misses when it is finally evaluated.
fib2 :: Int -> Int
fib2 n = fib' n 0 1
  where
    fib' 0 a _ = a
    fib' k a b = fib' (k - 1) b (a + b)

-- Strictness improves performance because lazy evaluation is not beneficial
-- here, notably resulting in poor performance.
fib3 :: Int -> Int
fib3 n = fib' n 0 1
  where
    fib' 0 !a !_ = a
    fib' !k !a !b = fib' (k - 1) b (a + b)
