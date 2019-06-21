import Data.Char
-- 1. a
-- 2. c
-- 3. b
-- 4. c

-- As - pattern
--1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xs') (y:ys)  
  | x==y      = isSubseqOf xs' ys
  | otherwise = isSubseqOf xs ys

--2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words
  where
    -- f xs@(x':xs') = (xs, toUpper x' : xs')
    f x = (x, capitalizeWord x)

-- Language Exercises
--1.
capitalizeWord :: String -> String
capitalizeWord (c:cs) = toUpper c : makeLower cs
  where
    makeLower [] = []
    makeLower (c:cs) = toLower c : makeLower cs

--2.
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . map f . words
  where
    f w = if elem '.' w then capitalizeWord w else w

capitalizeParagraph2 :: String -> String
capitalizeParagraph2 = unwords . map capitalizeWord . words