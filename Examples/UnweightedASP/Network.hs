-- Parse network files

module Network where

import Data.IntMap (IntMap, empty, insertWith)

type NodeId = Int
type Network = IntMap [NodeId]

edges :: [String] -> [(NodeId, NodeId)]
edges (x:y:ys) = (read x, read y) : edges ys
edges other = []

addEdge :: (NodeId, NodeId) -> Network -> Network
addEdge (from, to) =
  insertWith (++) from [to] . insertWith (++) to [from]

parseNetwork :: String -> Network
parseNetwork = foldr addEdge empty . edges . drop 2 . words
