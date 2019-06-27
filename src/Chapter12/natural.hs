assert :: Bool -> a -> a
assert False _ = error "Failed"
assert _ a = a

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

intToNat :: Integer -> Maybe Nat
intToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | x == 1 =  Just (Succ(Zero))
  | otherwise =  (intToNat (x-1))

-- assert (natToInteger Zero == 0) "j"
-- natToInteger Zero