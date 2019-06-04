module PoemLines where

-- 1.
myWords :: String -> [String]
myWords "" = []
myWords (' ':s) = myWords s
myWords s = takeWhile (/=' ') s : myWords (dropWhile (/= ' ' ) s)


-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n': s) = myLines s
myLines lines = takeWhile (/= '\n') lines : myLines (dropWhile (/= '\n') lines)

shouldEqual = [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]


-- 3.
mySplit :: Char -> String -> [String]
mySplit _ "" = []
mySplit ch s@(c:cs) = if c == ch then mySplit ch cs else takeWhile (/= ch) s : mySplit ch (dropWhile (/= ch) s)

main :: IO ()
main = 
  -- print $ "Equal? " ++ show (myLines sentences == shouldEqual)
  -- print $ "Equal? " ++ show (mySplit ' ' "jb gb ab" == ["jb", "gb", "ab"])
  print $ "Equal? " ++ show (mySplit '\n' sentences == shouldEqual)