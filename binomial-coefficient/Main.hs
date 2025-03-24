import Data.Array

main :: IO ()
main = do
  putStrLn "Start"
  -- Print binomial coefficient "n choose k." (nCk)

  -- Optimal
  print $ 30 `chooseOptimal` 10 -- 0th efficient

  -- Dynamic Programming (for practice)
  print $ 30 `chooseDPFast` 10 -- 1st efficient
  print $ 30 `chooseDP` 10 -- 2nd efficient

  -- No DP, Recursion only
  print $ 30 `chooseRecursive` 10 -- inefficient
  putStrLn "End"

-- O(2^n)
chooseRecursive :: Int -> Int -> Maybe Int
chooseRecursive n k
  | n < 0 || k < 0 = Nothing
  | n < k = Nothing
  | k > n - k = chooseRecursive n (n - k)
  | k == 0 = Just 1
  | otherwise = do
      a <- chooseRecursive (n - 1) (k - 1)
      b <- chooseRecursive (n - 1) k
      Just $ a + b

-- O(nk)
chooseDP :: Int -> Int -> Maybe Int
chooseDP n k
  | n < 0 || k < 0 = Nothing
  | n < k = Nothing
  | k > n - k = chooseDP n (n - k)
  | otherwise = Just $ dp ! n ! k
  where
    -- 2D, lazy array
    dp = array (0, n) [(i, createRow i) | i <- [0 .. n]]

    createRow 0 = array (0, 1) [(0, 1)]
    createRow n' =
      let colSize = min n' k
       in array (0, colSize) [(i, calcBinom n' i) | i <- [0 .. colSize]]

    calcBinom n' k'
      | k' == 0 = 1
      | k' == n' = 1
      | otherwise = let l = (dp ! (n' - 1)) in (l ! k') + (l ! (k' - 1))

-- O(nk)
chooseDPFast :: Int -> Int -> Maybe Int
chooseDPFast n k
  | n < 0 || k < 0 = Nothing
  | n < k = Nothing
  | k > n - k = chooseDPFast n (n - k)
  -- Using index to access linked list does not affect performance significantly
  -- since the index scan will only happen once during evaluation.
  | otherwise = Just $ dp !! n !! k
  where
    -- DP table implemented as a 2D, lazy list. (n choose k) corresponds to the
    -- n-th row and k-th column in the table. Each row is an infinite, lazy list.
    --
    -- Since each row is evaluated and accessed only once in sequential order,
    -- the GC may collect already used rows efficiently.
    dp :: [[Int]]
    dp = (1 : repeat 0) : buildDP dp
    buildDP (prevRow : rows) = zipWith (+) prevRow (0 : prevRow) : buildDP rows

-- O(n)
chooseOptimal :: Int -> Int -> Maybe Int
chooseOptimal n 0 = Just 1
chooseOptimal n k
  | n < 0 || k < 0 = Nothing
  | n < k = Nothing
  | k > n - k = chooseOptimal n (n - k)
  -- (n choose k) = ((n - k + 1) / k) (n choose (k - 1))
  | otherwise = do
      prev <- chooseOptimal n (k - 1)
      Just $ (prev * (n - k + 1)) `div` k
