module Lib where

import Control.Monad (replicateM)
import Data.List (repeat, sort, sortBy)
import Debug.Trace (trace)
import Graphics.Matplotlib
import System.Random

main :: IO ()
main = do
  xs <- replicateM 30 (randomRIO (0, 50) :: IO Int)
  ys <- replicateM 30 (randomRIO (0, 50) :: IO Int)
  let ps = zip xs ys
  let (xs', ys') = unzip $ convexHull ps
  onscreen $ scatter xs ys % plot xs' ys'
  return ()

mean :: (Fractional a1, Integral a2, Foldable t) => t a2 -> a1
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

sort' :: [[a]] -> [[a]]
sort' = sortBy (\a b -> if length a > length b then GT else LT)

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse c (x : xs) = x ++ c : intersperse c xs

data Direction = LEFT | RIGHT | STRAIGHT deriving (Show, Eq)

getDirection :: (Num a, Ord a, Show a) => (a, a) -> (a, a) -> (a, a) -> Direction
getDirection (ax, ay) (bx, by) (cx, cy)
  | det > 0 = LEFT
  | det < 0 = RIGHT
  | otherwise = STRAIGHT
 where
  det = (bx - ax) * (cy - by) - (cx - bx) * (by - ay)

getDirections :: (Num a, Ord a, Show a) => [(a, a)] -> [Direction]
getDirections (x : y : z : xs) = getDirection x y z : getDirections (y : z : xs)
getDirections _ = []

convexHull :: (Num a, Ord a, Show a) => [(a, a)] -> [(a, a)]
convexHull [] = []
convexHull [x] = [x]
convexHull [x, y] = [x, y]
convexHull xs = lower ++ upper
 where
  sorted = sort xs
  lower = go [] sorted
  upper = go [] $ reverse sorted
  go acc@(p2 : p1 : ps) (x : xs) =
    if getDirection p1 p2 x /= LEFT
      then go (tail acc) (x : xs)
      else go (x : acc) xs
  go acc (x : xs) = go (x : acc) xs
  go acc [] = reverse acc --reverse is not imp, but it makes cw hull (instead of ccw) due to appending x at head instead of tail

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines xs =
  let (pre, suf) = break isLineTerminator xs
   in pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        ('\r' : rest) -> splitLines rest
        ('\n' : rest) -> splitLines rest
        _ -> []
 where
  isLineTerminator c = c == '\r' || c == '\n'