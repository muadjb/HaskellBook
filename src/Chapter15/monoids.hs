import Data.Monoid
import Test.QuickCheck

--------------------------------------------
--  Laws
--------------------------------------------
semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--------------------------------------------
--  Trivial
--------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

--------------------------------------------
--  Identity
--------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdAssocS = Identity String -> Identity String -> Identity String -> Bool

checkIdentity :: IO ()
checkIdentity = do
  print $ Identity (Sum 42) <> Identity (Sum 5) == Identity (Sum 42 <> Sum 5)
  print $ Identity (Sum 42) <> Identity (Sum 5) == Identity (Sum 47)
  print $ Identity (Sum 46) <> Identity (Sum (-16)) == Identity (Sum 30)
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: IdAssocS)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mlr :: Identity String -> Bool)

--------------------------------------------
--  Two
--------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two c d = Two (a <> c) (b <> d)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  -- mempty = Two mempty mempty
  mempty = mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssocS = Two String String -> Two String String -> Two String String -> Bool

checkTwo :: IO ()
checkTwo = do
  print $ Two "jb" "jk" <> Two "gb" "ab" == Two "jbgb" "jkab"
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TwoAssocS)
  quickCheck (mli :: Two String String -> Bool)
  quickCheck (mlr :: Two String String -> Bool)

--------------------------------------------
--  BoolConj
--------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return (BoolConj True)), (1, return (BoolConj False)) ]

type BCAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
  print $ BoolConj True <> BoolConj True == BoolConj True
  print $ BoolConj True <> BoolConj False == BoolConj False
  print $ BoolConj False <> BoolConj True == BoolConj False
  print $ BoolConj False <> BoolConj False == BoolConj False
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BCAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)

--------------------------------------------
--  BoolDisj
--------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (1, return (BoolDisj True)), (1, return (BoolDisj False)) ]

type BDAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
  print $ BoolDisj True <> BoolDisj True == BoolDisj True
  print $ BoolDisj True <> BoolDisj False == BoolDisj True
  print $ BoolDisj False <> BoolDisj True == BoolDisj True
  print $ BoolDisj False <> BoolDisj False == BoolDisj False
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BDAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)

--------------------------------------------
--  Combine
--------------------------------------------
-- newtype Combine a b = Combine { unCombine :: (a -> b) }

-- instance Semigroup b => Semigroup (Combine a b) where
--   (Combine f) <> (Combine  g) = Combine (f <> g)

-- instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
--   mempty = mempty

-- genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
-- genFunc = arbitrary

-- genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
-- genCombine = do
--   f <- genFunc
--   return $ Combine f

-- instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = genCombine

-- -- type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool
-- type CombineIB = Combine Int Bool
-- type CombineAssoc = Combine Int Bool -> Combine Int Bool -> Combine Int Bool -> Bool

-- checkCombine :: IO ()
-- checkCombine = do
--   let sa = semigroupAssoc
--       mli = monoidLeftIdentity
--       mlr = monoidRightIdentity
--   quickCheck (sa :: CombineAssoc)
--   -- quickCheck (mli :: CombineIB -> Bool)
--   -- quickCheck (mlr :: CombineIB -> Bool)