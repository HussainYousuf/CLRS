module Sudoku where

isBlank :: Char -> Bool
isBlank = (== '0')

digits :: [Char]
digits = ['1' .. '4']

solve :: [[Char]] -> [[[Char]]]
solve = filter valid . expand . choices

valid :: Eq a => [[a]] -> Bool
valid g = all noDups (rows g) && all noDups (cols g) && all noDups (boxs g)

expand :: [[[a]]] -> [[[a]]]
expand = cp . map cp

choices :: [[Char]] -> [[[Char]]]
choices =
  map
    (map
       (\a ->
          if isBlank a
            then digits
            else [a]))

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x : ys | x <- xs, ys <- yss]
  where
    yss = cp xss

cols :: [[a]] -> [[a]]
cols []       = []
cols [xs]     = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs $ cols xss

noDups :: Eq a => [a] -> Bool
noDups []     = True
noDups (x:xs) = notElem x xs && noDups xs

rows :: a -> a
rows = id

boxs :: [[a]] -> [[a]]
boxs = map ungroup . ungroup . map cols . group . map group

ungroup :: [[a]] -> [a]
ungroup = concat

group :: [a] -> [[a]]
group [] = []
group xs = take 2 xs : group (drop 2 xs)

sample :: [[Char]]
sample = replicate 4 $ ['1'..'4'] ++ replicate 3 '0'
