--1.
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x

-- 3.
--a) Blah or Woot
--b) No instance of Num
--c) No instance of Ord

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"