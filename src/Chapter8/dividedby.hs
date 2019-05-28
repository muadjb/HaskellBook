
-- dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
        --  | n == 0 = ( count, 0)
         | abs n < abs d = (signum num * signum denom * count, n)
         | otherwise = go (abs n - abs d) d (count + 1)


data DivResult = Result Integer | DivByZero deriving Show

-- dividedBy2 :: (Integral a) => a -> a -> (DivResult, Integer)
dividedBy2 :: Integer ->Integer -> (DivResult, Integer) 
dividedBy2 num denom = go num denom 0
  where go n   d count
         | d == 0 = (DivByZero, 0)
         | abs n < abs d = (Result (signum num * signum denom * count), n)
         | otherwise = go (abs n - abs d) d (count + 1)
