mTh1 x y z = x * y * z
mTh2 x y  = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

-- I don't undertsand why mTh4 has type Integer -> ... instead of Num a like 1, 2, and 3.

-- 2. type of mTh3 should be d) Num a with two arguments

-- 3.
addOne = \x -> x + 1

-- a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b)
-- addFive x y = (if x > y then y else x) + 5
addFive  = \x -> \y -> (if x > y then y else x) + 5

-- c)
-- mflip f = \x -> \y -> f y x
mflip f x y = y x