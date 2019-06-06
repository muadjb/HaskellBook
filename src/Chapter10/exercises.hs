-- 1.
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

-- a.
makeTuples :: [Char] -> [Char] -> [Char] -> [(Char, Char, Char)] 
makeTuples s1 v s2 = [(x, y, z) | x <- s1, y <- v, z <- s2 ]

-- b.
makePTuples :: [Char] -> [Char] -> [Char] -> [(Char, Char, Char)] 
makePTuples s1 v s2 = [(x, y, z) | x <- s1, y <- v, z <- s2, x=='p' ]

-- c.
nouns :: [String]
nouns = ["dog", "tree", "mug", "computer"]

verbs :: [String]
verbs = ["dance", "sleep", "laugh", "look"]

makeSentences :: [String] -> [String] -> [String] -> [(String, String, String)] 
makeSentences s1 v s2 = [(x, y, z) | x <- s1, y <- v, z <- s2 ]


--2. 
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

--3. 
seekritFunc2 :: String -> Double
seekritFunc2 x = (/) (sum (map (read::String->Double) (words x))) (fromIntegral (length (words x)))


-- Rewrite wtih folds
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
-- myAnd = foldr f True
--   where f cur acc = acc && cur


-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny:: (a -> Bool) -> [a] -> Bool
myAny p = foldr f False
  where f cur acc = p cur || acc

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr f False
  where f cur acc = cur == e || acc

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e = myAny (==e)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr g []
  where g cur acc = f cur : acc

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr f []
  where f cur acc = if p cur then cur : acc else acc

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
-- how do I use composition?  concat . f doesn't work
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr g []
  where g cur acc = f cur ++ acc

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compare (x:xs) = foldr f x xs
  where f cur acc = if compare cur acc == GT then cur else acc

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compare (x:xs) = foldr f x xs
  where f cur acc = if compare cur acc == LT then cur else acc