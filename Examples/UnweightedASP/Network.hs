-- Parse network files

module Network where

import Prelude
import Text.Printf
import Data.IntMap (IntMap, empty, insertWith, keys, toList)
import Data.Bits

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

genHexFiles :: String -> String -> IO ()
genHexFiles filename destDir = do
  contents <- readFile filename
  let net = parseNetwork contents
  let neighbours = concat [dsts ++ [src] | (src, dsts) <- toList net]
  writeFile (destDir ++ "neighbours.hex") $ unlines
    [printf "%x" n |  n <- neighbours]
  let states = [1 `shiftL` src | src <- keys net] :: [Integer]
  writeFile (destDir ++ "state.hex") $ unlines
    [printf "%x" s |  s <- states]
