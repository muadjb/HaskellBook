data Possibly a = Nope | Yep a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ Nope = Nope
  fmap f (Yep a) = Yep (f a)
  -- fmap f (Second b) = Second (f b)