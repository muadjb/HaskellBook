tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where xLast = fst (divMod x 10)
        d     = snd (divMod xLast 10)

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        d     = xLast `mod` 10


-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = case b of
  True -> y
  False -> x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y b 
  | b == True = y
  | otherwise = x


-- 3.
g :: (a -> b) -> (a,c) -> (b,c)
g f (x,y) = (f x, y)


-- 4.
data Product a b = Product a b deriving (Eq, Show)
productUnpack :: Product a b -> (a, b)
productUnpack (Product x x) = (x, x)