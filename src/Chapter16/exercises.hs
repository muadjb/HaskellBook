{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

-------------
-- Valid?
-------------
--1.  data Bool = False | True
-- No because it is of kind *

--2. Yes
data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)


--3. Yes
data BoolAndMaybeSomethingElse a = Falsish a | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Falsish a) = Falsish (f a)
  fmap f (Truish a) = Truish (f a)

--4. Yes???
newtype Mu f = Inf { outF :: f (Mu f)}
-- I'm confused by the use of 'f' inside the record syntax.
-- The 'f' inside the parens and paired with Mu is a type argument.
-- The 'f' outside the parens looks like a function 
-- I think becuase Mu takes an argument it is * -> * and therefor a Functor.

--5. No - lacks type argument
data D = D (Array Word Word) Int Int

-------------
-- Rearrange
-------------
--1.  data Sum a b = First a | Second b
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b


-- 2.  data Company a b c = DeepBlue a c | Something b
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something  (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.  data More a b = L a b a | R b a b
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-------------
-- Instances
-------------
--1. 
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

--2. 
data K a b = K a

instance Functor (K x) where
  fmap _ (K a) = K a
  -- fmap _ (K b) = K b

--3. 
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

--4. 
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst x) where
  fmap f (GoatyConst b) = GoatyConst (f b)

--5. 
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor x => Functor (LiftItOut x) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)