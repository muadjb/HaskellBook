-- 1.
-- id :: a -> a
-- *

-- 2.
-- r :: a -> f a
-- a = *
-- f = *


-- String processing
notThe :: String -> Maybe String
notThe s = case s of
  "the" -> Nothing
  otherwise -> Just s 

theToA :: String -> String
theToA s = case notThe s of
  Nothing -> "a"
  otherwise -> s

replaceThe :: String -> String
replaceThe =  unwords . (fmap theToA) . words


-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s = 0