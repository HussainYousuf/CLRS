{-# LANGUAGE NamedFieldPuns #-}

module ImpGraph where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Text.Printf (printf)

type Label = String
data Node a = Node
    { nodeLabel :: Label
    , nodeContent :: a
    , isVisited :: Bool
    }
data Edge b = Edge
    { edgeLabel :: Label
    , edgeContent :: b
    }
    deriving (Show)

-- data Tree s a b = Tree (Node s a) [(Edge b, Label)]
-- newtype Graph s a b = Graph (Map Label (Tree s a b))

-- data Tree a b = Tree

newtype Graph a b
    = Graph
        [ ( IORef (Node a)
          , [(Edge b, IORef (Node a))]
          )
        ]

buildGraph :: IO (Graph [Char] b)
buildGraph = do
    nodeA <- newIORef $ Node{nodeLabel = "A", nodeContent = "A", isVisited = False}
    nodeB <- newIORef $ Node{nodeLabel = "B", nodeContent = "B", isVisited = False}
    return $
        Graph
            [
                ( nodeA
                , [(undefined, nodeB)]
                )
            ,
                ( nodeB
                , [(undefined, nodeA)]
                )
            ]

dfs :: Graph String b -> IO ()
dfs (Graph []) = return ()
dfs (Graph ((node, links) : xs)) = do
    Node{nodeLabel, nodeContent, isVisited} <- readIORef node
    unless isVisited $ do
        printf "nodeLabel: %s\tnodeContent: %s\n" nodeLabel nodeContent
        writeIORef node (Node{nodeLabel, nodeContent, isVisited = True})
    case links of
        (_, successor) : ys -> do
            dfs $ Graph $ (successor, ys) : xs
        [] -> do
            dfs $ Graph xs
    return ()

main :: IO ()
main = buildGraph >>= dfs
