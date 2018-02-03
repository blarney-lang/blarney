-- Calculate average shortest path of undirected graph/network

import Blarney
import Network
import Data.IntMap (IntMap, keys, (!), fromList)

-- Max number of nodes in network (must be a power of 2)
type MaxNodes = 16

-- Reaching vector, hot for each node that reaches
type Reaching = Bit MaxNodes

-- Number of clock cycles that have elapsed
type Time = Bit 32

-- Holds the sum of path lengths
type Count = Bit 32

data Node =
  Node {
    update   :: Time -> Reaching -> RTL ()
  , reaching :: Reaching
  , total    :: Count
  , changed  :: Bit 1
  }

makeNode :: NodeId -> RTL Node
makeNode id = do
  -- Bit vector of all nodes reaching this node
  -- Initially, node reaches itself
  reaching <- makeReg (fromInteger (2 ^ toInteger id))
  -- Track the sum of the lengths of paths reaching this node
  total <- makeReg 0
  -- Has this node changed?
  changed <- makeWire 0

  -- Update method
  let update t r = do
        reaching <== val reaching .|. r
        let newOnes = countOnes (r .&. inv (val reaching))
        when (newOnes .!=. 0) $ do
          changed <== 1
        total <== val total + t * zeroExtend newOnes

  return (Node update (val reaching) (val total) (val changed))

-- Network compiler
compile :: Network -> RTL ()
compile net = do
  -- Create each node
  nodes <- mapM makeNode (keys net)
  -- Mapping from node id to node
  let nodeMap = fromList (zip (keys net) nodes)
  -- Timer
  timer <- makeReg 1
  timer <== val timer + 1
  -- Call update method for each node
  forM (zip (keys net) nodes) $ \(id, node) -> do
    let newReaching = orList [reaching (nodeMap!n) | n <- net!id]
    update node (val timer) newReaching
  -- Terminate when nothing changes
  when (inv (orList (map changed nodes))) $ do
    --display "Sum = " (sumList (map total nodes))
    display "diameter = " (val timer - 1)
    finish

main :: IO ()
main = readFile "n1.edges"
   >>= netlist . compile . parseNetwork
   >>= writeNetlist "/tmp/asp.net"
