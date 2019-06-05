myzip :: [a] -> [b] -> [(a,b)]
myzip  [] _ = []
myzip  _ [] = []
myzip  (a:as) (b:bs) = (a,b) : myzip as bs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ [] = []
myzipWith f (a:as) (b:bs) = f a b  : myzipWith f as bs

toPair :: a -> b -> (a,b)
toPair a b = (a,b)

myzip2 :: [a] -> [b] -> [(a,b)]
myzip2 = myzipWith toPair