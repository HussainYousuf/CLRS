{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Function (fix)
import Data.List (elemIndex, foldl')
import Data.Maybe (isJust)
import qualified Data.MemoCombinators as Memo
import qualified Lib
import Text.RawString.QQ (r)

main :: IO ()
main = Lib.main

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

class Heap heap where
  {-# MINIMAL
    isEmpty
    , empty
    , (singleton | insert)
    , (fromList | (singleton, merge))
    , (insert | (merge, singleton))
    , (merge | (decompose, insert))
    , (decompose | (findMax, deleteMax))
    #-}

  type Elem heap

  isEmpty :: heap -> Bool

  findMax :: heap -> Maybe (Elem heap)
  findMax = (fst <$>) <$> decompose

  empty :: heap

  singleton :: Elem heap -> heap
  singleton = (`insert` empty)

  fromList :: [Elem heap] -> heap
  fromList xs =
    case go (map singleton xs) of
      [heap] -> heap
      [] -> empty
      _ -> error "Fatal error. Did not converge to a single heap."
   where
    go [] = []
    go [x] = [x]
    go (x : y : rest) = go (merge x y : go rest)

  decompose :: heap -> Maybe (Elem heap, heap)
  decompose heap = case (findMax heap, deleteMax heap) of
    (Just heapMax, Just heapRest) -> Just (heapMax, heapRest)
    (Nothing, Nothing) -> Nothing
    (Just _, Nothing) -> error "Impossible happened. There is a max but the heap is empty."
    (Nothing, Just _) -> error "Impossible happened. Heap is non-empty but there is a max."

  insert :: Elem heap -> heap -> heap
  insert elem = merge $ singleton elem

  deleteMax :: heap -> Maybe heap
  deleteMax = (snd <$>) <$> decompose

  merge :: heap -> heap -> heap
  merge heap1 heap2 = case decompose heap1 of
    Just (maxElem, restHeap) -> merge restHeap $ maxElem `insert` heap2
    Nothing -> heap2

  rank :: heap -> Int
  rank = undefined

data LeftistHeap a = Leaf | Node a Int (LeftistHeap a) (LeftistHeap a)
  deriving (Eq, Show)

instance (Ord a) => Heap (LeftistHeap a) where
  type Elem (LeftistHeap a) = a

  rank Leaf = 0
  rank (Node _ r _ _) = r

  merge h1 Leaf = h1
  merge Leaf h2 = h2
  merge h1@(Node x _ l1 r1) h2@(Node y _ l2 r2)
    | x <= y = mkNode x l1 $ merge r1 h2
    | otherwise = mkNode y l2 $ merge r2 h1
   where
    mkNode elem left right
      | rank left >= rank right = Node elem (rank right + 1) left right
      | otherwise = Node elem (rank left + 1) right left

  decompose Leaf = Nothing
  decompose (Node a _ l r) = Just (a, merge l r)

  empty = Leaf

  isEmpty = (== empty)

  singleton x = Node x 1 Leaf Leaf

heapSort :: forall a. (Ord a) => [a] -> [a]
heapSort xs = toList $ decompose xs'
 where
  toList Nothing = []
  toList (Just (x, xs)) = x : toList (decompose xs)
  xs' = fromList xs :: LeftistHeap a

isBalanced :: String -> Bool
isBalanced = null . foldl' op []
 where
  op ('(' : xs) ')' = xs
  op ('[' : xs) ']' = xs
  op ('{' : xs) '}' = xs
  op xs x
    | x `elem` "()[]{}" = x : xs
    | otherwise = xs

str :: [Char]
str = [r| [{ "name": "John", "age": 30 }, { "name": "Kyle", "age": 31 }]|]

rmDups :: Eq a => [a] -> [a]
rmDups = foldr (\x xs -> if x `elem` xs then xs else x : xs) []

memoize :: (Int -> Integer) -> (Int -> Integer)
memoize f = (map f [0 ..] !!)

rodCut :: [Integer] -> Int -> Integer
rodCut ps = fix (memoize . rodCut')
 where
  rodCut' f 0 = 0
  rodCut' f n = maximum [ps !! (i -1) + f (n - i) | i <- [1 .. n]]

fib1 n
  | n < 2 = n
  | otherwise = fib1 (n -1) + fib1 (n -2)

fib2 = 0 : 1 : zipWith (+) fib2 (tail fib2)

fib3 = fix (memoize . fib)
 where
  fib f 0 = 0
  fib f 1 = 1
  fib f n = f (n -1) + f (n -2)