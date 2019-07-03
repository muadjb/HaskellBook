module Optional where

import Data.Monoid

data Optional a = Some a | None deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) x None = x
  (<>) None y = y
  (<>) (Some x) (Some y) = Some (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = None

main :: IO ()
main = do
  print $ Some (Sum 1) `mappend` Some (Sum 1)
  print $ Some (Product 2) `mappend` Some (Product 4)
  print $ mappend (Some (Product 2)) None
  print $ mappend (Some [1]) None
  print $ mappend None (Some ("jb"))
  -- print $ mappend None None