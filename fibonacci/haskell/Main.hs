module Main where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let num = readMaybe =<< listToMaybe args
  case num of
    Just num' -> print $ fib3 num'
    Nothing -> exitFailure

-- Elegant example demonstrating the nature of Haskell's lazy evaluation.
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- Inefficient due to linear space complexity. Since we only need a single term
-- of the Fibonacci sequence, this approach creates numerous intermediate
-- values which cannot be garbage collected.
fib1 :: Int -> Int
fib1 n = fibs !! n

-- Lazy-evaluated function arguments lead to poor performance. as the thunk,
-- an unevaluated expression that can be represented as a deep tree, grows
-- and becomes increasingly complex, allocating a lot of memory in the heap,
-- which may result in many cache misses when it is finally evaluated.
fib2 :: Int -> Int
fib2 n = fib' n 0 1
  where
    fib' 0 a _ = a
    fib' k a b = fib' (k - 1) b (a + b)

-- Strictness improves performance because lazy evaluation is not beneficial here,
-- notably resulting in poor performance.
fib3 :: Int -> Int
fib3 n = fib' n 0 1
  where
    fib' 0 !a !_ = a
    fib' !k !a !b = fib' (k - 1) b (a + b)