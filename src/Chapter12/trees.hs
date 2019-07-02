data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- myIterate :: (a -> a) -> a -> [a]
-- myIterate f a = a : myIterate f (f a)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (x, y, z) ->  Node Leaf y Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n 
  | n < 1 = unfold (const Nothing) n
  | otherwise = unfold (\x -> Just(x, n-1, x)) n