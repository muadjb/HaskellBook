import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a,s) }

combineMems f g s = (a <> b, s'')
    where
      (a, s') = g s
      (b, s'') = f s'

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ combineMems f g

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  -- print $ (rmzero :: (Sum Int, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  print $ runMem (f' <> f') 0