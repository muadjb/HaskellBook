lefts' :: [Either a b] -> [a]
-- lefts' [] = []
-- lefts' (x:xs) = 
--   case x of
--     Right _ -> []
--     Left a -> a : (lefts' xs)

lefts' = foldr f []
  where 
    f e b = case e of
      Left l -> l : b
      otherwise -> b
      
rights' :: [Either a b] -> [b]
rights' = foldr right []
  where 
    right (Right r) b = r:b
    right _ b = b

partitonEithers' :: [Either a b] -> ([a], [b])
partitonEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right r) = Just (f r)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ g (Right r) = g r

eitherMaybe2' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe2' g = either' (\_ -> Nothing) (Just . g)