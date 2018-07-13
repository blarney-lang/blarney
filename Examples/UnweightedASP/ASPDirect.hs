import Blarney
import Network
import qualified Data.IntMap as M

-- Graphs

type Vertex = Int 
type Graph = M.IntMap [Vertex]

-- Sets

type MaxVertices = 87

type Set = Bit MaxVertices

empty :: Set
empty = 0

singleton :: Vertex -> Set
singleton v = fromInteger (2 ^ toInteger v)

size :: Set -> Bit 32
size = zeroExtend `o` countOnes

union :: Set -> Set -> Set
union = (.|.)

unions :: [Set] -> Set
unions = foldr union empty

-- Processing

combine :: Graph -> (Vertex -> Set) -> Vertex -> Set
combine g f v = unions (singleton v : map f (g M.! v))

initial :: Graph -> [Set]
initial g = map singleton (M.keys g)

step :: Graph -> [Set] -> [Set]
step g l = map (combine g (l !!)) (M.keys g)

levels :: Graph -> [Set]
levels g = outs
  where outs = zipWith reg (initial g) (step g outs)

-- Shortest paths

diffs :: [Set] -> [Bit 32]
diffs sets = zipWith (-) newSizes oldSizes
  where
    newSizes = map size sets
    oldSizes = map (reg 0) newSizes
 
ssp :: Graph -> (Bit 1, Bit 32, Bit 32)
ssp g = (done, diam, total)
  where
    ds    = diffs (levels g)
    done  = andList [d .==. 0 | d <- ds]
    total = reg 0 (total + sum [diam * d | d <- ds])
    diam  = reg 0 (diam + 1)

-- Compile graph to RTL

compile :: Graph -> RTL ()
compile g = do
  let (done, diam, total) = ssp g
  when done $ do
    display "Sum = " total
    finish

-- Main function
main :: IO ()
main = do
  net <- parseNetwork `fmap` readFile "n2.edges"
  emitVerilogTop (compile net) "top" "ASPDirect-Verilog/"
