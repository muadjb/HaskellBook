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

  -- need folding function :: a -> b -> b
  -- b is Bool in this case so a -> Bool -> Bool

  -- f = (==e) :: a -> Bool
  -- g = (||) :: Bool - Bool -> Bool
  -- g .f 
  --          a -> Bool
  --               Bool -> Bool -> Bool
  -- These cancel ^^^^^

  -- g .f :: a ->          Bool -> Bool

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e = foldr ((||) . (==e)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e = myAny (==e)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
-- myMap f = foldr g []
--   where g cur acc = f cur : acc

  -- need folding function :: a -> b -> b
  -- b is [b] in this case so a -> [b] -> [b]

  -- f :: a -> b
  -- g = (:) :: b -> [b] -> [b]
  -- g .f 
  --          a -> b
  --               b -> [b] -> [b]
  -- These cancel ^^

  -- g .f :: a ->       [b] -> [b]


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
-- squishMap f = foldr g []
  -- where g cur acc = f cur ++ acc

  -- need folding function :: a -> b -> b
  -- b is [b] in this case so a -> [b] -> [b]

  -- f :: a -> [b]
  -- g = (++) :: [b] -> [b] -> [b]
  -- g .f 
  --          a -> [b]
  --               [b] -> [b] -> [b]
  -- These cancel ^^^^

  -- g .f :: a ->       [b] -> [b]

squishMap f = foldr ((++) . f) []

 -- f takes one argument, but foldr sends two arguments (two unary functions) to the folding function.  What happens to the second argument/function?

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