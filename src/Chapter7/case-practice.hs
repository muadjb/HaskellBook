-- 1.
functionC x y = case x > y of
  True -> x
  False -> y

-- 2.
ifEvenAdd2 n = case mod n 2 == 0 of
  True -> n + 2
  False -> n

  -- 3.
nums x = case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1