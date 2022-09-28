module LeftistHeap where

import           Data.Maybe (fromJust)

data Tree a
  = Node Int a (Tree a) (Tree a)
  | Empty
  deriving (Show, Eq)

merge :: Ord t => Tree t -> Tree t -> Tree t
merge Empty t2 = t2
merge t1 Empty = t1
merge t1@(Node _ x left1 right1) t2@(Node _ y left2 right2)
  | x <= y = makeTree x left1 (merge right1 t2)
  | otherwise = makeTree y left2 (merge right2 t1)

makeTree :: a -> Tree a -> Tree a -> Tree a
makeTree x left right
  | leftRank >= rightRank = Node (rightRank + 1) x left right
  | otherwise = Node (leftRank + 1) x right left
  where
    leftRank = rank left
    rightRank = rank right
    rank Empty          = 0
    rank (Node n _ _ _) = n

insert :: Ord a => a -> Tree a -> Tree a
insert x h = merge h $ Node 1 x Empty Empty

findMin :: Tree a -> Maybe a
findMin (Node _ x _ _) = Just x
findMin Empty          = Nothing

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Node _ _ left right) = merge left right
deleteMin Empty                 = Empty

sample1 :: (Ord a, Enum a, Num a) => Tree a
sample1 = foldr insert Empty [9,8 .. 0]

toList :: Tree b -> [(Int, b)]
toList Empty = []
toList (Node r x left right) = (r, x) : f (toList left) (toList right)
  where
    f left []    = left
    f [] right   = right
    f left right = left ++ right

sort :: Ord a => Tree a -> [a]
sort Empty = []
sort h     = fromJust (findMin h) : sort (deleteMin h)
