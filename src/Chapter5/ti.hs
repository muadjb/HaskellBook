-- typeInference1.hs
module TypeInference1 where

-- f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat x = x ++ "yo"

myMult x = (x / 3) * 5

myCom x = x > (length [1 .. 10])

myAlph x = x < 'z'
