{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- 1.
(* 9) 6 -- 54 :: Num a => a

head [(0, "doge"), (1, "kitteh")] -- (0, "doge") :: Num a => (a, [Char])

head [(0 :: Integer, "doge"), (1, "kitteh")] -- (0, "doge") :: (Integer, [Char])

if False
  then True
  else False -- False :: Bool

length [1, 2, 3, 4, 5] -- 5 :: Int

(length [1, 2, 3, 4]) > (length "TACOCAT") -- False :: Bool

-- 2. w :: Num a => a
-- 3. z :: Num a => a -> a
-- 4. f :: Fractional a => a
-- 5. f :: String
myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))
