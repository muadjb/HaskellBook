-- 2.
appendE :: String -> String
appendE x = x ++ "!"

getFourth x = x !! 4

drop9 x = drop 9 x

-- 3.
getThirdChar :: String -> Char
getThirdChar x = x !! 2

-- 4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- 5.
reverseJ :: String
reverseJ =
  let curry = take 5 "Curry is awesome!"
      awsome = drop 9 "Curry is awesome!"
      isStart = drop 5 "Curry is awesome!"
      is = take 4 isStart
  in concat [awsome, is, curry]

reverseW :: String
-- reverseW = concat [awesome, is, curry]
reverseW = awesome ++ is ++ curry
  where curry = take 5 "Curry is awesome"
        awesome = drop 9 "Curry is awesome"
        isStart = drop 5 "Curry is awesome"
        is = take 4 isStart
    