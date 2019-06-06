fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

-- 1.
twentyFibs :: [Integer]
twentyFibs = take 20 fibs

-- 2.
fibsLessThan100 :: [Integer]
-- fibsLessThan100 = [x | x <- fibs, x < 100]
fibsLessThan100 = takeWhile (<100) fibs
