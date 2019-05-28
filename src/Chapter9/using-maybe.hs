safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail (x:[]) = Nothing
safetail (_:xs) = Just xs

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x
