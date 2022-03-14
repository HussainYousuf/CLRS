-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Function (fix)
import Data.List (elemIndex, foldl', maximumBy)
import Data.Maybe (Maybe (Nothing), fromJust, isJust)
import qualified Data.MemoCombinators as Memo
import Data.Ord (comparing)
import Debug.Trace (trace)
import qualified Lib
import Text.RawString.QQ (r)

main :: IO ()
main = return ()

-- insertionSort
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
 where
  insert :: Ord a => a -> [a] -> [a]
  insert x [] = [x]
  insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys

-- mergeSort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
 where
  mid = length xs `div` 2
  (left, right) = splitAt mid xs
  merge :: Ord a => [a] -> [a] -> [a]
  merge xs [] = xs
  merge [] xs = xs
  merge xs'@(x : xs) ys'@(y : ys)
    | x <= y = x : merge xs ys'
    | otherwise = y : merge xs' ys

-- linearSearch
linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch _ [] = Nothing
linearSearch y (x : xs) = if y == x then elemIndex x xs else linearSearch y xs

-- binarySearch
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch y xs = binarySearch' y xs xs
 where
  binarySearch' :: Ord a => a -> [a] -> [a] -> Maybe Int
  binarySearch' _ [] _ = Nothing
  binarySearch' y [x] orig = if y == x then elemIndex x orig else Nothing
  binarySearch' y xs orig
    | y == x = elemIndex x orig
    | y < x = binarySearch' y left orig
    | otherwise = binarySearch' y right orig
   where
    mid = length xs `div` 2
    (left, x : right) = splitAt mid xs

-- 2.3.7
e2_3_7 :: (Num a, Ord a) => a -> [a] -> Maybe (a, a)
e2_3_7 t xs = foldr (\a b -> if isJust a then a else b) Nothing $ f xs'
 where
  xs' = mergeSort xs
  f [] = []
  f (x : xs) = binarySearch' x xs : f xs
  binarySearch' _ [] = Nothing
  binarySearch' y [x] = if x + y == t then Just (y, x) else Nothing
  binarySearch' y xs
    | x + y == t = Just (y, x)
    | x + y < t = binarySearch' y right
    | otherwise = binarySearch' y left
   where
    mid = length xs `div` 2
    (left, x : right) = splitAt mid xs

maxSubArray :: (Num a, Ord a) => [a] -> [a]
maxSubArray xs = maxSubArray' xs [] [] 0 0
 where
  maxSubArray' [] _ maxSub _ maxSum = if maxSum <= 0 then [] else reverse maxSub
  maxSubArray' (x : xs) sub maxSub acc maxSum =
    if sum > 0
      then
        if sum > maxSum
          then maxSubArray' xs sub' sub' sum sum
          else maxSubArray' xs sub' maxSub sum maxSum
      else maxSubArray' xs [] maxSub 0 maxSum
   where
    sum = acc + x
    sub' = x : sub

-- 13,-3,-25,20,-3,-16,-23,18,20,-7,12,-5,-22,15,-4,7

isBalanced :: String -> Bool
isBalanced = null . foldl' op []
 where
  op ('(' : xs) ')' = xs
  op ('[' : xs) ']' = xs
  op ('{' : xs) '}' = xs
  op xs x
    | x `elem` "()[]{}" = x : xs
    | otherwise = xs

-- str :: [Char]
-- str = [r| [{ "name": "John", "age": 30 }, { "name": "Kyle", "age": 31 }]|]

rmDups :: Eq a => [a] -> [a]
rmDups = foldr (\x xs -> if x `elem` xs then xs else x : xs) []

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

rodCut' ps 0 = 0
rodCut' ps n = let x = maximum [ps !! (i -1) + rodCut' ps (n - i) | i <- [1 .. n]] in trace (show x) x

rodCut :: [Integer] -> Int -> Integer
rodCut ps = fix (memoize . rodCut')
 where
  rodCut' f 0 = 0
  rodCut' f n = maximum [ps !! (i -1) + f (n - i) | i <- [1 .. n]]

rodCutWithSol :: [Integer] -> Int -> Integer
rodCutWithSol ps n = let x = rodCut ps n in trace (let res = reverse $ subSetSum ps' x in show (map (fromJust . flip elemIndex ps') res) ++ "\n" ++ show res) x
 where
  ps' = take n ps

(+++) :: [a] -> [a] -> [a]
(+++) _ xs@(_ : _) = xs
(+++) xs@(_ : _) _ = xs
(+++) _ _ = []

subSetSum :: (Ord t, Num t) => [t] -> t -> [t]
subSetSum xs n = subSetSum' xs n [] 0
 where
  subSetSum' input target result acc
    | acc == target = result
    | null input || acc > target = []
    | otherwise = (if x <= 0 then [] else subSetSum' input target (x : result) (x + acc)) +++ subSetSum' xs target result acc
   where
    (x : xs) = input

fib1 :: (Ord a, Num a) => a -> a
fib1 n
  | n < 2 = n
  | otherwise = fib1 (n -1) + fib1 (n -2)

fib2 :: [Integer]
fib2 = 0 : 1 : zipWith (+) fib2 (tail fib2)

fib3 :: Int -> Integer
fib3 = fix (memoize . fib)
 where
  fib f 0 = 0
  fib f 1 = 1
  fib f n = f (n -1) + f (n -2)

longest :: Ord a => [[a]] -> [a]
longest = maximumBy (comparing length)

-- longest common subsequence
lcs :: [Char] -> [Char] -> [Char]
lcs = Memo.memo2 (Memo.list Memo.char) (Memo.list Memo.char) lcs'
 where
  lcs' [] _ = []
  lcs' _ [] = []
  lcs' xxs@(x : xs) yys@(y : ys)
    | x == y = x : lcs xs ys
    | otherwise = longest [lcs xs yys, lcs xxs ys]

-- longest palindrome subsequence
lps :: [Char] -> [Char]
lps xs = lcs xs $ reverse xs

perms :: [a] -> [[a]]
perms = foldr (concatMap . insert) [[]]
 where
  insert :: a -> [a] -> [[a]]
  insert a [] = [[a]]
  insert a xss@(x : xs) = (a : xss) : map (x :) (insert a xs)

-- foldr f e · concat = foldr (flip $ foldr f) e
-- concat = foldr (++) []

-- foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
-- (++) xs ys = foldr (:) ys xs

-- foldr f e (xs ++ ys) = foldr f e (foldr (:) ys xs) = foldr f (foldr f e ys) xs
-- foldr f e · concat = foldr f e . foldr (++) [] = foldr ? (foldr f e []) = foldr (flip $ foldr f) (e)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

wrap :: a -> [a]
wrap = (: [])

-- unwrap :: [a] -> a
-- unwrap [x] = x

single :: [a] -> Bool
single [x] = True
single _ = False

integer :: Num a => [a] -> a
integer = foldl' (\a x -> a * 10 + x) 0

fraction :: Fractional a => [a] -> a
fraction = foldr (\x a -> a / 10 + x) 0

-- quickSort :: Ord a => [a] -> [a]
-- quickSort [] = []
-- quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
--   where
--     smaller = filter (<=x) xs
--     larger = filter (>x) xs

-- type Node = Int
-- type Edge b = [(b, Node)]
-- type Context a b = (Edge b, Node, a, Edge b)
-- data Graph a b = Context a b & Graph | Empty

fibfast n = fib' 0 1 n
 where
  fib' a b n
    | n == 0 = a
    | otherwise = fib' b (a + b) (n -1)

-- Osaki Purely Functional DataStructures
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xxs@(_ : xs) = xxs : suffixes xs

data Tree a = Node a (Tree a) (Tree a) | Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty =  Node x Empty Empty
insert x tree@(Node a left right)
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right) 
  | otherwise = tree