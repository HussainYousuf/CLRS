{-# LANGUAGE RankNTypes #-}

module Graph where

import Data.Array (Array, Ix, accumArray, bounds, elems, indices, (!))
import Data.Tuple (swap)

main :: IO ()
main = print "asd"

-- a can be thought of as vertex

type Graph a = Array a [a]

vertices :: (Ix a) => Graph a -> [a]
vertices = indices

type Edge a = (a, a)

edges :: (Ix a) => Graph a -> [Edge a]
edges graph = [(v1, v2) | v1 <- vertices graph, v2 <- graph ! v1]

outdegree :: Graph a -> Array a Int
outdegree = fmap length

type Bounds a = (a, a)

buildG :: (Ix a) => Bounds a -> [Edge a] -> Graph a
buildG = accumArray (flip (:)) []

reverseE :: (Ix a) => Graph a -> [Edge a]
reverseE = map swap . edges

transposeG :: (Ix a) => Graph a -> Graph a
transposeG graph = buildG (bounds graph) $ reverseE graph

indegree :: (Ix a) => Graph a -> Array a Int
indegree = outdegree . transposeG

undirected :: (Ix a) => Graph a -> Graph a
undirected graph = buildG (bounds graph) (edges' ++ map swap edges')
  where
    edges' = edges graph

data Tree a = Node a (Forest a) deriving (Show)
type Forest a = [Tree a]

dfs :: Graph a -> [a] -> Forest a
dfs = undefined

dff :: (Ix a) => Graph a -> Forest a
dff graph = dfs graph (vertices graph)

preorder :: Tree a -> [a]
preorder (Node a xs) = a : preorderF xs

preorderF :: Forest a -> [a]
preorderF = concatMap preorder

preOrderG :: (Ix a) => Graph a -> [a]
preOrderG = preorderF . dff

postorder :: Tree a -> [a]
postorder (Node a xs) = postorderF xs ++ [a]

postorderF :: Forest a -> [a]
postorderF = concatMap postorder

postOrderG :: (Ix a) => Graph a -> [a]
postOrderG = postorderF . dff

generate :: (Ix a) => Graph a -> a -> Tree a
generate graph vertex = Node vertex $ map (generate graph) $ graph ! vertex

graph =
    buildG
        ('a', 'j')
        [ ('a', 'j')
        , ('a', 'g')
        , ('b', 'i')
        , ('b', 'a')
        , ('c', 'h')
        , ('c', 'e')
        , ('e', 'j')
        , ('e', 'h')
        , ('e', 'd')
        , ('f', 'i')
        , ('g', 'f')
        , ('g', 'b')
        ]