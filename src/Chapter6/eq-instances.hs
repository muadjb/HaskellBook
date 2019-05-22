-- 1.
data TisAnInteger = TisAn Integer deriving Show

instance Eq TisAnInteger where
   TisAn a == TisAn b = a == b


-- 2.
data TwoIntegers = Two Integer Integer deriving Show

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'


-- 3.
data StringOrInt = IsInt Int | IsString String deriving Show

instance Eq StringOrInt where
  (==)  (IsInt x) (IsInt x') = x == x'
  (==)  (IsString s) (IsString s') = s == s'
  (==)  _ _ = False
    

-- 4.
data Pair a = Pair a a deriving Show

instance Eq a => Eq (Pair a) where
  (==)  (Pair x y) (Pair x' y') = x == x' && y == y'


-- 5.
data Tuple a b = Tuple a b deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==)  (Tuple a b) (Tuple a' b') = a == a' && b == b'

  
-- 6.
data Which a = ThisOne a | ThatOne a deriving Show

instance Eq a => Eq (Which a) where
  (==)  (ThisOne a) (ThisOne a') = a == a'
  (==)  (ThatOne a) (ThatOne a') = a == a'
  (==)  _ _ = False


-- 7.
data EitherOr a b = Hello a | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==)  _ _ = False