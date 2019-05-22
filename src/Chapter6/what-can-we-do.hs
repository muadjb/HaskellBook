data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)
data Papu2 = Papu21 String Bool deriving (Eq, Show)

-- 1.
-- phew = Papu "chases" False
 -- ERROR: string and bool aren't equal to Rocks and Yeah

phew = Papu21 "chases" False

-- 2.
truth = Papu (Rocks "chases") (Yeah True)

-- 3.
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
 -- ERROR: No Ord
