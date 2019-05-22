h :: (Num a, Num b) => a -> b -> b
h = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

-- Parametricity Exercise
-- 2.
hypo2 :: a -> a -> a
hypo2 x y = x
hypo2 x y = y

-- 3.
hypo3 :: a -> b -> b
hypo3 x y = x
hypo3 x y = y

-- Multiple Choice
-- 1. c
-- 2. a
-- 3. c
-- 4. c