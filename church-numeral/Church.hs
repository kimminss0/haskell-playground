module Church
  ( Church (..),
    zero,
    one,
    Church.succ,
    Church.pred,
    isZero,
  )
where

newtype Church = Church {runChurch :: forall a. (a -> a) -> a -> a}

instance Show Church where
  show (Church f) = show $ f (1 +) 0

zero :: Church
zero = Church (\f x -> x)

one :: Church
one = Church.succ zero

isZero :: Church -> Bool
isZero (Church n) = n (const False) True

succ :: Church -> Church
succ (Church n) = Church (\f x -> f (n f x))

pred :: Church -> Church
pred (Church n) = Church $ \f x -> n (\g h -> h (g f)) (const x) id

instance Num Church where
  (+) (Church m) (Church n) = Church $ \f x -> m f (n f x)
  (*) (Church m) (Church n) = Church $ \f -> m (n f)
  (-) cm (Church n) = n Church.pred cm

  -- Only non-negative integers are supported.
  abs = id
  signum = const one
  fromInteger n
    | n < 0 = error "fromInteger: negative"
    | n == 0 = zero
    | otherwise = Church.succ $ fromInteger (n - 1)

instance Eq Church where
  (==) cm cn = cm <= cn && cn <= cm

instance Ord Church where
  (<=) cm cn = isZero (cm - cn)
