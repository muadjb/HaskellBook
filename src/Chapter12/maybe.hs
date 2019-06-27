isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a] 
maybeToList Nothing = []  
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where
     f Nothing b = b
     f (Just a) b = a : b
-- catMaybes x = [a | (Just a) <- x]

flipMaybes :: [Maybe a] -> Maybe [a]
flipMaybes [] = Just []
flipMaybes (Nothing:_) = Nothing
flipMaybes (Just x:xs) = 
  case flipMaybes xs of
    Nothing -> Nothing
    Just ys -> Just (x:ys)