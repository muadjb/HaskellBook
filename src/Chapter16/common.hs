-- Maybe (incorrect implementation because it lacks a type argument)
-- data FixMePls = FixMe | Pls deriving (Eq, Show)

-- instance Functor FixMePls where
--   fmap = error "it doesn't matter, it won't compile"

  -- * Expected kind `* -> *', but `FixMePls' has kind `*'
  -- * In the first argument of `Functor', namely `FixMePls'
  --   In the instance declaration for `Functor FixMePls'

-- Maybe (correct implementation)
data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

  -- * Expected kind `* -> *', but `FixMePls' has kind `*'
  -- * In the first argument of `Functor', namely `FixMePls'
  --   In the instance declaration for `Functor FixMePls'


data CountingBad a = Heisenbad Int a deriving (Eq, Show)
instance Functor CountingBad where
  fmap f (Heisenbad n a) = Heisenbad (n+1) (f a)

data CountingGood a = Heisengood Int a deriving (Eq, Show)
instance Functor CountingGood where
  fmap f (Heisengood n a) = Heisengood (n) (f a)

testCounting :: IO ()
testCounting = do
  print $ fmap head $ Heisenbad 4 "jb"
  print $ fmap head $ Heisengood 4 "jb"

  let u = "Uncle"
      oneWhoKnocksBad = Heisenbad 0 u
      oneWhoKnocksGood = Heisengood 0 u
      j = (++ " Jesse")
      l = (++ " lol")
      f = j . l
  print $ fmap j oneWhoKnocksBad
  print $ fmap f oneWhoKnocksBad
  print $ fmap j . fmap l $ oneWhoKnocksBad

  print $ fmap j oneWhoKnocksGood
  print $ fmap f oneWhoKnocksGood
  print $ fmap j . fmap l $ oneWhoKnocksGood


replaceWithP :: a -> Char
replaceWithP = const 'p'

common :: IO ()
common = do
  print $ fmap replaceWithP $ Just 10
  print $ fmap replaceWithP Nothing
  print "---"
  print $ fmap replaceWithP [1..5]
  print $ fmap replaceWithP "Ave"
  print $ fmap (+1) []
  print $ fmap replaceWithP []
  print "---"
  print $ fmap replaceWithP (10,20)
  print $ fmap replaceWithP (10,"woo")
  print "---"
  let tossEmOne = fmap (+1) negate
  print $ tossEmOne 10
  print $ tossEmOne (-10)
  print "---"

n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms :: [Maybe [Char]]
lms = [ave, n, w]
tripMap = fmap . fmap . fmap

runlms :: IO ()
runlms = do
  print $ replaceWithP lms
  print $ fmap replaceWithP lms
  print $ (fmap . fmap) replaceWithP lms
  print $ tripMap replaceWithP lms

-- fmap . fmap typecheck
-- replaceWithP :: a -> Char
-- lms :: List (Maybe String)
-- fmap replaceWithP lms :: 
--      a == List
--      a -> Char    List (Maybe String) :: List(Char)

-- fmap replaceWithP (fmap replaceWithP lms) :: 
--      a == List
--      a -> Char    List (Maybe String) :: List(Char)


ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]

upDown :: IO ()
upDown = do
  print $ fmap replaceWithP lmls
  print $ (fmap . fmap) replaceWithP lmls
  print $ tripMap replaceWithP lmls
  print $ (tripMap . fmap) replaceWithP lmls

-- head lmls :: List (Maybe (List (List (Char))))
-- fmap lmls                      :: List (Char)                       = "ppp"
-- fmap . fmap lmls               :: List (Maybe (Char))               = [Just p, Nothing, Just p]
-- fmap . fmap . fmap lmls        :: List (Maybe (List (Char)))        = [Just ["pp"], N, Just 'p']
-- fmap . fmap . fmap . fmap lmls :: List (Maybe (List (List (Char)))) = [Just ["pp", "pp", N, ??? ]]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = (fmap . fmap) replaceWithP

thriceLifted :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted