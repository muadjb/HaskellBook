data MyReader e a = MyReader (e -> a)

instance Functor (MyReader e) where
  fmap  = readerMap
  
-- readerMap :: (a -> b) -> MyReader (e a) -> MyReader (e b)
-- readerMap :: (a -> b) -> MyReader e a -> MyReader e b
readerMap g (MyReader f) = MyReader (g.f)

-- From https://www.mjoldfield.com/atelier/2014/08/monads-reader.html
readerMap2 :: (a -> b) -> MyReader e a -> MyReader e b
readerMap2 f (MyReader x) = MyReader $ \e -> (f.x) e