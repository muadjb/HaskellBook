import Data.List

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

countVowels :: String -> Int
countVowels s = length $ (intersect s vowels)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s > (length s - countVowels s) = Nothing
  | otherwise = Just (Word' s)