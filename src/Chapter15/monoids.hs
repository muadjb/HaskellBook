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

-- type IdAssocI = Identity Sum Int -> Identity Sum Int -> Identity Sum Int -> Bool
type IdAssocS = Identity String -> Identity String -> Identity String -> Bool

checkIdentity :: IO ()
checkIdentity = do
  print $ Identity (Sum 42) <> Identity (Sum 5) == Identity (Sum 42 <> Sum 5)
  print $ Identity (Sum 42) <> Identity (Sum 5) == Identity (Sum 47)
  print $ Identity (Sum 46) <> Identity (Sum (-16)) == Identity (Sum 30)
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  -- quickCheck (sa :: IdAssocI)
  quickCheck (sa :: IdAssocS)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mlr :: Identity String -> Bool)
