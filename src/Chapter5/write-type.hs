myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))

-- myFunc2 :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
-- myFunc2 xToY yToZ _ (a, x) = (a, (yToZ . xToY x))
-- myFunc3 :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
-- myFunc3 xToY yToZ _ (a, x) = (a, ((xToY . yToZ) x))
-- 1.
i :: a -> a
i a = a

-- 2.
c :: a -> b -> a
c a b = a

-- 3.
c'' :: b -> a -> b
c'' b a = b

-- Yes, c and c'' are equivalent
-- 5.
r :: [a] -> [a]
-- r x = tail x
-- r x = init x
r x = reverse x

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

-- 7.
a :: (a -> c) -> a -> a
a aToC a = a

-- 8.
a' :: (a -> b) -> a -> b
a' x a = x a
