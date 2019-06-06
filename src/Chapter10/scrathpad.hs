
  -- g = (++) :: [b] -> [b] -> [b]foldr :: (a -> b -> b) -> b -> [a] -> b

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

initial = []
input = [4,5,6]

f :: a -> [b]
f x = [1,x,3]

-- 1.
f 4 (foldr f [] [5,6])

-- 2.
f 4 (f 5 (foldr f [] [6]))

-- 3.
f 4 (f 5 (f 6 (foldr f [] [])))

-- 4.
f 4 (f 5 (f 6 ([])))

-- 5.
f 4 (f 5 ([1,6,3] ([])))


  -- need folding function :: a -> b -> b
  -- b is a in this case so a -> a -> a

  -- f :: a -> a -> Ordering
  -- g = ??? :: Ordering -> a
  -- g .f 

  --         a -> a -> Ordering
  --                   Ordering -> a
  -- These cancel     ^^^^^^^^^

  -- g .f :: a -> a             -> a