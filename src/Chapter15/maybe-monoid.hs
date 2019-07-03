import Control.Monad
import Data.Monoid
import Test.QuickCheck

--------------------------------------------
--  Optional
--------------------------------------------
data Optional a = Some a | None deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) x None = x
  (<>) None y = y
  (<>) (Some x) (Some y) = Some (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = None

--------------------------------------------
--  Laws
--------------------------------------------
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--------------------------------------------
--  Broken Boolean
--------------------------------------------
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools
  -- mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   -- let ma = monoidAssoc
--   --     mli = monoidLeftIdentity
--   --     mlr = monoidRightIdentity
--   quickCheck (monoidAssoc :: BullMappend)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)
--   -- quickCheck (ma :: BullMappend)
--   -- quickCheck (mli :: Bull -> Bool)

--------------------------------------------
--  Maybe Another Monoid
--------------------------------------------
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  First' None <> First' None = First' None
  First' None <> y = y
  x <> y = x

instance Monoid (First' a) where
  mempty = First' {getFirst' = None}

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (2, return $ First' (Some a)), (1, return $ First' None) ]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

checkMaybe :: IO ()
checkMaybe = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)