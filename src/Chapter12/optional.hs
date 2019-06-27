data Optional a = Some a | None

instance Semigroup (Optional a) where
  (<>) = undefined

instance Monoid (Optional a) where
  mempty = None
  mappend = undefined

newtype Jsum n = Sum n

instance Num n => Monoid (Jsum n) where
  mempty = Sum 0
  mappend = Sum x <> Sum y = Sum (x+y)