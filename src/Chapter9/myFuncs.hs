myAnd :: [Bool] -> Bool
myAnd [] = True 
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False 
-- myOr (x:xs) = if x == True then True else myOr xs
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False 
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False 
myElem a (x:xs) = if a == x then True else myElem a xs

-- Not working
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a = any (== a) 

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)
-- myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain a = squishMap (\x -> x++[]) a

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:ys) = if f x y == GT then myMaximumBy f (x:ys) else myMaximumBy f (y:ys) 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:y:ys) = if f x y == LT then myMinimumBy f (x:ys) else myMinimumBy f (y:ys) 

myMaximum :: Ord a => [a] -> a
myMaximum a= myMaximumBy compare a

myMinimum a = myMinimumBy compare a
