data Optional a = Some a | None deriving (Eq, Show)

instance Semigroup (Optional a) where
  (<>) = undefined

instance Monoid a => Monoid (Optional a) where
  mempty = None

  -- mappend :: (Optional a) -> (Optional a) -> (Optional a)
  mappend None _ = None
  mappend _ None = None
  -- mappend (Some x) (Some y) = (Some x)
  mappend (Some x) (Some y) = Some(x <> y)

-- newtype Jsum n = Sum n

-- instance Num n => Monoid (Jsum n) where
--   mempty = Sum 0
--   mappend = Sum x <> Sum y = Sum (x+y)