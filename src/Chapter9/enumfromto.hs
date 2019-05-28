eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  | a == b = [a]
  | a > b = []
  | a < b = a:[b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a == b = [a]
  | a > b = []
  | a < b = a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a == b = [a]
  | a > b = []
  | a < b = a : eftInt (succ a) b

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a == b = [a]
  | a > b = []
  | a < b = a : eftChar (succ a) b
