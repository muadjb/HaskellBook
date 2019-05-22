-- type Hole = undefined
hole = undefined

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
-- f :: a -> b
-- x :: a
-- y :: b
--
-- hole :: a == b
chk f x y = f x == y


-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
-- f :: a -> b
-- i :: Integer
-- x :: a
--
-- hole :: b
arith f i x = f x + fromIntegral i