import Data.Monoid
import Test.QuickCheck

--------------------------------------------
--  Laws
--------------------------------------------
semiAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semiAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

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

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial =
  quickCheck (semiAssoc :: TrivAssoc)

--------------------------------------------
--  Identity
--------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdAssocI = Identity Int -> Identity Int -> Identity Int -> Bool
type IdAssocS = Identity String -> Identity String -> Identity String -> Bool

-- Can't make this test fail
jb :: (Eq m, Semigroup m) => m -> m -> Bool
jb a b = Identity a <> Identity b == Identity (a <> b)

checkIdentity :: IO ()
checkIdentity = do
  print $ Identity (Sum 42) <> Identity (Sum 5) == Identity (Sum 42 <> Sum 5)
  print $ Identity (Sum 46) <> Identity (Sum (-16)) == Identity (Sum 46 <> Sum (-16))
  -- This should fail but it doesn't
  quickCheck (jb :: Identity (Sum Int) -> Identity (Sum Int) -> Bool)
  -- quickCheck (semiAssoc :: IdAssocI)
  -- quickCheck (semiAssoc :: IdAssocS)

--------------------------------------------
--  Two
--------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two c d = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssocS = Two String String -> Two String String -> Two String String -> Bool

checkTwo :: IO ()
checkTwo = do
  print $ Two "jb" "jk" <> Two "gb" "ab" == Two "jbgb" "jkab"
  quickCheck (semiAssoc :: TwoAssocS)

--------------------------------------------
--  Three
--------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssocS = Three String String String -> Three String String String -> Three String String String -> Bool

checkThree :: IO ()
checkThree = do
  print $ Three "jb" "jk" "do" <> Three "gb" "ab" "it" == Three "jbgb" "jkab" "doit"
  quickCheck (semiAssoc :: ThreeAssocS)

--------------------------------------------
--  Four
--------------------------------------------
data Four a b c d =  Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssocS = Four String String String String -> Four String String String String -> Four String String String String -> Bool

checkFour :: IO ()
checkFour = do
  print $ Four "jb" "jk" "do" "boo" <> Four "gb" "ab" "it" "hoo" == Four "jbgb" "jkab" "doit" "boohoo"
  quickCheck (semiAssoc :: FourAssocS)

--------------------------------------------
--  BoolConj
--------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return (BoolConj True)), (1, return (BoolConj False)) ]

type BCAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
  print $ BoolConj True <> BoolConj True == BoolConj True
  print $ BoolConj True <> BoolConj False == BoolConj False
  print $ BoolConj False <> BoolConj True == BoolConj False
  print $ BoolConj False <> BoolConj False == BoolConj False
  quickCheck (semiAssoc :: BCAssoc)

--------------------------------------------
--  BoolDisj
--------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (1, return (BoolDisj True)), (1, return (BoolDisj False)) ]

type BDAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
  print $ BoolDisj True <> BoolDisj True == BoolDisj True
  print $ BoolDisj True <> BoolDisj False == BoolDisj True
  print $ BoolDisj False <> BoolDisj True == BoolDisj True
  print $ BoolDisj False <> BoolDisj False == BoolDisj False
  quickCheck (semiAssoc :: BDAssoc)

--------------------------------------------
--  Or
--------------------------------------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _     = Snd x
  _     <> y     = y
  -- Fst x <> Fst y = Fst y
  -- Fst x <> Snd y = Snd y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a), (1, return $ Snd b) ]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

checkOr :: IO ()
checkOr = do
  -- print $ (Fst 4) <> (Fst 5) == (Fst 5)
  print $ Fst 4 <> Snd 7 == Snd 7
  print $ Snd 7 <> Fst 4 == Snd 7
  -- print $ Snd "a" <> Snd "b" == Snd "a"
  quickCheck (semiAssoc :: OrAssoc)

--------------------------------------------
--  Combine
--------------------------------------------
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- instance Show (Combine a b) where
--   show (Combine _) = "Combine instance"

-- instance Eq (Combine a b) where
--   (Combine f) == (Combine g) = _x

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine  g) = Combine (f <> g)

-- genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
-- genFunc = arbitrary

-- genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
-- genCombine = do
--   f <- genFunc
--   return $ Combine f

-- instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = genCombine


-- type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

-- checkCombine :: IO ()
-- checkCombine = do
--   quickCheck (semiAssoc :: CombineAssoc)

-- from https://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type
funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) = property $ \a -> f a === g a

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

combineAssoc :: (Arbitrary a, Show a, Semigroup b, Eq b, Show b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))

checkCombine = quickCheck $ \(Fn f) (Fn g) (Fn h) ->
  (combineAssoc :: CombineAssoc Int String) (Combine f) (Combine g) (Combine h)

--------------------------------------------
--  Comp
--------------------------------------------
newtype Comp a = Comp { unComp :: (a -> a) }

-- instance Show (Comp a) where
--   show (Comp _) = "Comp instance"

instance Semigroup a => Semigroup (Comp a) where
  -- (Comp f) <> (Comp  g) = Comp (f <> g)
  (Comp f) <> (Comp  g) = Comp (f . g)

-- from https://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type
combFunEquality :: (Arbitrary a, Show a, Eq a) => Comp a -> Comp a -> Property
combFunEquality (Comp f) (Comp g) = property $ \a -> f a === g a

type CompAssoc a = Comp a -> Comp a -> Comp a -> Property

combAssoc :: (Arbitrary a, Semigroup a, Show a, Eq a) => CompAssoc a
combAssoc f g h = ((f <> g) <> h) `combFunEquality` (f <> (g <> h))

checkComp = quickCheck $ \(Fn f) (Fn g) (Fn h) ->
  (combAssoc :: CompAssoc (Sum Int)) (Comp f) (Comp g) (Comp h)

-- instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
--   arbitrary = do
--       f <- Test.QuickCheck.arbitrary
--       return (Comp f)

-- type CompAssoc2 = String Comp String -> Comp String -> Comp String -> Bool
-- combAssoc2 :: (Semigroup a, Eq a) => CompAssoc a
-- combAssoc2 f g h = ((f <> g) <> h) `combFunEquality` (f <> (g <> h))

-- checkComp2 = quickCheck (semiAssoc :: CompAssoc2)