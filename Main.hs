module Main where

import Data.List (elemIndex)
import Data.Maybe (isJust)

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- insertionSort
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-- mergeSort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge xs'@(x : xs) ys'@(y : ys)
    | x <= y = x : merge xs ys'
    | otherwise = y : merge xs' ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs

-- linearSearch
linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch _ [] = Nothing
linearSearch y (x : xs) = if y == x then elemIndex x xs else linearSearch y xs

-- binarySearch
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch y xs = binarySearch' y xs xs

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
    f (x : xs) = binarySearch'' x xs : f xs
    binarySearch'' _ [] = Nothing
    binarySearch'' y [x] = if x + y == t then Just (y, x) else Nothing
    binarySearch'' y xs
        | x + y == t = Just (y, x)
        | x + y < t = binarySearch'' y right
        | otherwise = binarySearch'' y left
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