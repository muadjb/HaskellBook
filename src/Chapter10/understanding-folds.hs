foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs


-- 1. b & c

-- 2
-- foldl (flip (*)) 1 [1..3]
-- 1:1:2:3
-- ((1*1)*2)*3
-- (1*2)*3
-- 2*3
-- 6

--3. c

--4. a

--5. 
--a) 
-- foldr (++) ["woot", "Woot", "w"]
foldr (++) [] ["woot", "Woot", "w"]

--b) 
-- foldr max [] "fear is little death"
foldr max [] ["fear", "is", "little", "death"]

--c)
-- foldr and True [False, True]
foldr (&&) True [False, True]

--d)
-- foldr (||) True [False, True]
foldr (||) False [False, True]

--e)
-- foldl ((++).show) "" [1..5]
foldl (flip ((++).show)) "" [1..5]

--f)
-- foldr const 'a' [1..5]
foldr const 1 [1..5]

--g)
-- foldr const 0 "tacos"
foldr const 'j' "tacos"

--h)
-- foldl (flip const) 0 "tacos"
foldl (flip const) 'j' "burritos"

--i)
foldl (flip const) 33 [1..5]