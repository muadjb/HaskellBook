-- 1.
myWords :: String -> [String]
myWords " " = []
myWords s = takeWhile (/=' ') s : myWords (dropWhile (==' ') s)