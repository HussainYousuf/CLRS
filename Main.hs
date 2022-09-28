module Main where

import           Data.List (foldl', partition)

-- import           Chp1
-- import qualified Data.List   as L
-- import qualified LeftistHeap as LH
-- import qualified Sudoku as SU
main :: IO ()
main = print "main"

disjoint :: Eq a => [a] -> [a] -> [a]
disjoint xs ys =
  foldr (\a bs -> filter (/= a) bs) xs ys ++
  foldr (\a bs -> filter (/= a) bs) ys xs

picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [x : zs | (x, ys) <- picks xs, zs <- perms2 ys]

perms1 :: [a] -> [[a]]
perms1 = foldr (concatMap . insert) [[]]

insert :: t -> [t] -> [[t]]
insert x []     = [[x]]
insert y (x:xs) = (y : x : xs) : map (x :) (insert y xs)

-- insert x xs = map (x:) $ foldr (\a bs -> insert a bs) [[]] xs
takeWhile' :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhile' p =
  foldr
    (\x xs ->
       if p x
         then x : xs
         else [])
    []

dropWhileEnd' :: Foldable t => (a -> Bool) -> t a -> [a]
dropWhileEnd' p =
  foldr
    (\x xs ->
       if p x && null xs
         then []
         else x : xs)
    []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = reverse . dropWhileEnd' p . reverse

integer :: [Integer] -> Integer
integer = fst . foldr (\x (sum, y) -> (x * 10 ^ y + sum, y + 1)) (0, 0)

fraction :: [Double] -> Double
fraction xs =
  fst $ foldr (\x (sum, y) -> (x / 10 ^ y + sum, y - 1)) (0, length xs) xs

apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = apply (n - 1) f (f x)

collapse :: [[Int]] -> [Int]
collapse xss = lambda xss id 0
  where
    lambda [] f _ = f []
    lambda (xs:xss) f accSum
      | accSum > 0 = f []
      | otherwise = lambda xss (f . (xs ++)) (accSum + sum xs)

minfree :: [Int] -> Int
minfree xs' = minfree' xs' 0

minfree' :: [Int] -> Int -> Int
minfree' [] a = a
minfree' xs' start = if length xs == b then minfree' ys b  else minfree' xs start 
  where
    b = (start + length xs') `div` 2
    as = [start..b-1]
    bs = [b..]
    (xs, ys) = partition (<b) xs'