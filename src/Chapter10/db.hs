import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
  DbNumber 9001,
  DbNumber 1,
  DbString "hello world",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]


isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

accumDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
accumDbDate (DbDate cur) acc = cur:acc
accumDbDate _ acc = acc

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr accumDbDate [] items

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer] 
filterDbNumber items = [x | (DbNumber  x) <- items]

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
-- mostRecent items = maximum $ filterDbDate items
mostRecent = maximum . filterDbDate 

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral (sumDb items) / fromIntegral (length $ filterDbNumber items)