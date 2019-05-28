-- 2.
sumToN :: (Eq a, Num a) => a -> a
sumToN 1 = 1
sumToN n = n + sumToN (n-1)

-- sumToN2 :: (Eq a, Num a) => a -> a
-- sumToN2 n = n
-- sumToN2 n = n + sumToN2 (n-1)

-- 3.
multR :: (Integral a) => a -> a -> a
multR _ 0 = 0
multR x 1 = x
multR x y = x + multR x (y-1)