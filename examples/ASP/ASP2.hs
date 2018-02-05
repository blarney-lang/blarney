import Blarney
import Network
import Data.IntMap (IntMap, (!), keys)

-- Graphs

type Vertex = Int 
type Graph = IntMap [Vertex]

-- Sets

type MaxVertices = 2048

type Set = Bit MaxVertices

empty :: Set
empty = 0

singleton :: Vertex -> Set
singleton v = fromInteger (2 ^ toInteger v)

size :: Set -> Bit 32
size = zeroExtend . countOnes

union :: Set -> Set -> Set
union = (.|.)

unions :: [Set] -> Set
unions = foldr union empty

-- Processing

combine :: Graph -> (Vertex -> Set) -> Vertex -> Set
combine g f v = unions (singleton v : map f (g!v))

initial :: Graph -> [Set]
initial g = map singleton (keys g)

step :: Graph -> [Set] -> [Set]
step g l = map (combine g (l !!)) (keys g)

levels :: Graph -> [Set]
levels g = outs
  where outs = zipWith reg (initial g) (step g outs)

-- Diameter

diameter :: Graph -> (Bit 1, Bit 32)
diameter g = (done, diam)
  where
    done     = andList (zipWith (.==.) newSizes oldSizes)
    newSizes = map size (levels g)
    oldSizes = map (reg 0) newSizes
    diam     = reg 0 (1 + diam)

-- Compile graph to RTL

compile :: Graph -> RTL ()
compile g = do
  let (done, diam) = diameter g
  when done $ do
    display "Diameter = " (diam - 1)
    finish

-- Main function
main :: IO ()
main = readFile "n4.edges"
   >>= netlist . compile . parseNetwork
   >>= writeNetlist "/tmp/asp.net"
