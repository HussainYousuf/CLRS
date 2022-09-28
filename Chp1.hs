module Chp1 where

import           Data.Char (isAlpha, toLower)
import           Data.List (group, sort)

commonWords :: Int -> String -> [String]
commonWords n =
  map snd .
  take n .
  reverse .
  sort .
  map (\xs -> (length xs, head xs)) .
  group . sort . map (map toLower) . filter (all isAlpha) . words

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

convert6 :: Int -> String
convert6 n
  | m == 0 = convert3 h
  | h == 0 = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where
    (m, h) = (n `div` 1000, n `rem` 1000)
    link h
      | h < 100 = " and "
      | otherwise = " "

convert :: Int -> String
convert = convert6

anagrams :: Int -> [String] -> String
anagrams n = unlines . map sort . filter (\w -> length w == n)
