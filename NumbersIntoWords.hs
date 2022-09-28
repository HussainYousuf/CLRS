module NumbersIntoWords where

units :: [String]
units =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

teens :: [String]
teens =
  [ "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  ]

tens :: [String]
tens =
  ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

convert1 :: Int -> String
convert1 n = units !! n

convert2 :: Int -> String
convert2 n
  | t == 0 = convert1 n
  | t == 1 = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ " " ++ convert1 u
  where
    (t, u) = (n `div` 10, n `rem` 10)

convert3 :: Int -> String
convert3 n
  | h == 0 = convert2 t
  | t == 0 = convert1 h ++ " hundred"
  | otherwise = convert1 h ++ " hundred and " ++ convert2 t
  where
    (h, t) = (n `div` 100, n `rem` 100)
