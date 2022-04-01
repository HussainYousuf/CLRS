{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Alga where

import Data.Map as M

-- a is the datastructure
-- e is the label
class Graph a e | a -> e where
    type Vertex a
    vertex :: Vertex a -> a
    connect :: e -> a -> a -> a

-- a is the vertex
-- e is the label
newtype LabelledAdjacencyMap a e = LAM
    { getMap :: M.Map a (M.Map a e)
    }

fromEdge :: (Ord a) => (a, a, e) -> LabelledAdjacencyMap a e
fromEdge (v1, v2, e) = LAM $ M.fromList [(v1, M.singleton v2 e), (v2, M.empty)]

fromEdges :: (Ord a) => [(a, a, e)] -> LabelledAdjacencyMap a e
fromEdges edges = LAM $ M.unionsWith mappend $ Prelude.map (getMap . fromEdge) edges

getVertices :: LabelledAdjacencyMap a e -> [a]
getVertices = M.keys . getMap
-- overlay :: Ord a => LabelledAdjacencyMap e a -> LabelledAdjacencyMap e a -> LabelledAdjacencyMap e a
-- overlay (LAM map1) (LAM map2) = LAM $ M.unionWith mappend map1 map2

instance (Ord a) => Graph (LabelledAdjacencyMap a e) e where
    type Vertex (LabelledAdjacencyMap a e) = a
    vertex a = LAM $ M.singleton a M.empty
    connect e (LAM map1) (LAM map2) = LAM $ M.unionsWith mappend $ map1 : map2 : [updatedMap1]
      where
        edgesFromMap1to2 = M.fromSet (const e) $ M.keysSet map2
        updatedMap1 = M.fromSet (const edgesFromMap1to2) $ M.keysSet map1

-- main = print "Alga"