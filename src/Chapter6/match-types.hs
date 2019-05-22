import Data.List (sort)
-- 1. NO
-- i :: Num a => a
-- i :: a
-- i = 1


-- 2. NO
-- f2 :: Float
-- f2 :: Num a => a
-- f2 = 1.0

-- 3. YES
-- f3 :: Float
-- f3 :: Fractional a => a
-- f3 = 1.0


-- 4. YES
-- f4 :: Float
f4 :: RealFrac a => a
f4 = 1.0

-- 5. YES
-- freud5 :: a -> a
freud5 :: Ord a => a -> a
freud5 x = x

-- 6. YES
-- freud6 :: a -> a
freud6 :: Int -> Int
freud6 x = x

-- 7. NO
-- myX7 = 1 :: Int
-- -- signumd :: Int -> Int
-- signumd :: a -> a
-- signumd x = myX7

-- 8. NO
-- myX8 = 1 :: Int
-- -- signumd8 :: Int -> Int
-- signumd8 :: Num a => a -> a
-- signumd8 x = myX8

-- 9.  YES
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10. YEs
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)
