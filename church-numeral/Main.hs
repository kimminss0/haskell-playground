module Main where

import Church
import Data.Function (fix)

fac' :: (Church -> Church) -> Church -> Church
fac' f n
  | isZero n = one
  | otherwise = n * f (Church.pred n)

fac :: Church -> Church
fac = fix fac'

main :: IO ()
main = do
  putStr "Enter a (small) number: "
  n <- readLn
  putStr $ "fac " ++ show n ++ " = "
  print $ fac (fromInteger n)
