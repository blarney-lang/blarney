import Blarney
import Data.IntMap (IntMap, empty, insertWith, keys, (!), fromList)

-- Max number of nodes
type MaxNodes = 32

-- Networks
type NodeId = Int
type Network = IntMap [NodeId]

-- Network parser
edges :: [String] -> [(NodeId, NodeId)]
edges (x:y:ys) = (read x, read y) : edges ys
edges other = []

addEdge :: (NodeId, NodeId) -> Network -> Network
addEdge (from, to) =
  insertWith (++) from [to] . insertWith (++) to [from]

parseNetwork :: String -> Network
parseNetwork = foldr addEdge empty . edges . drop 2 . words

-- Hardware types
type Count = Bit 32
type Reaching = Bit MaxNodes

data Node =
  Node {
    update   :: Count -> Reaching -> RTL ()
  , reaching :: Reaching
  , total    :: Count
  , changed  :: Bit 1
  }

makeNode :: NodeId -> RTL Node
makeNode id = do
  -- Bit vector of all nodes reaching this node
  -- Initially, node reaches itself
  reaching :: Reg Reaching <- makeReg (fromInteger (2 ^ toInteger id))
  -- Track the sum of the lengths of paths reaching this node
  total <- makeReg 0
  -- Has this node changed?
  changed <- makeWire 0

  -- Update method
  let update t r = do
        reaching <== r
        let newOnes = countOnes (r .&. inv (val reaching))
        when (newOnes .!=. 0) $ do
          changed <== 1
        total <== val total + t * newOnes

  return (Node update (val reaching) (val total) (val changed))

-- Network compiler
compile :: Network -> RTL ()
compile net = do
  -- Create each node
  nodes <- mapM makeNode (keys net)
  -- Mapping from node id to node
  let nodeMap = fromList (zip (keys net) nodes)
  -- Timer
  timer :: Reg Count <- makeReg 0
  timer <== val timer + 1
  -- Call update method for each node
  forM (zip (keys net) nodes) $ \(id, node) -> do
    let newReaching = orList [reaching (nodeMap!n) | n <- net!id]
    update node (val timer) newReaching
  -- Terminate when nothing changes
  when (inv (orList (map changed nodes))) $ do
    display "Sum = " (sumList (map total nodes))
    finish

main :: IO ()
main = do
  contents <- readFile "n1.edges"
  let net = parseNetwork contents
  nl <- netlist (compile net)
  writeVerilog "/tmp/asp.v" nl
