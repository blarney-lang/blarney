import Network
import Blarney
import Blarney.RAM

-- These parameters are for a 4096 node engine with up to 2^17 edges
#define LogMaxNeighbours      17
#define LogMaxVertices        12
#define LogMaxVerticesPlusOne 13
#define MaxVertices           4096

-- This is a general ASP-finding engine,
-- but for the purposes of testing,
-- let's assume the n5 network:
#define numVertices   3487
#define numNeighbours 119385

-- Types
type NeighbourId = Bit LogMaxNeighbours
type VertexId    = Bit LogMaxVertices
type VertexState = Bit MaxVertices
type VertexCount = Bit LogMaxVertices
type StateAddr   = Bit LogMaxVerticesPlusOne

-- Pipelined population count with n pipeline stages
popCount :: (KnownNat n, 1 <= n) => Bit 1 -> Bit (2^n) -> (Bit 1, Bit n)
popCount go vec = (done, count)
  where
    n       = widthOf count
    done    = foldr reg go (replicate n 0)
    count   = tree1 add [zeroExtend (getBit i vec) | i <- [0..2^n-1]]
    add a b = reg 0 (a+b)

-- Average shortest path engine
makeASPEngine :: RTL ()
makeASPEngine = do
  -- RAM storing concatenated neighbour arrays (one for each vertex)
  -- A self-loop denotes the end of a neighbour array for the current vertex
  neighboursMem :: RAM NeighbourId VertexId <- makeRAMInit "neighbours.hex"

  -- RAM storing vertex state
  stateMem :: RAM StateAddr VertexState <- makeRAMInit "state.hex"

  -- =========
  -- Data path
  -- =========

  -- Trigger for the population count pipeline
  popCountGo :: Reg (Bit 1) <- makeDReg 0

  -- Trigger for the fetch-accumulate pipeline
  fetchAccumulateGo :: Reg (Bit 1) <- makeDReg 0

  -- Index register for neighbours memory
  i :: Reg NeighbourId <- makeReg

  -- Current source vertex
  src :: Reg VertexId <- makeReg

  -- We tread the state memory as a double buffer
  -- Which is the active buffer?
  active :: Reg (Bit 1) <- makeReg

  -- Input to population-count pipeline
  popCountIn :: Reg VertexState <- makeReg

  -- Stall wires for fetch-accumulate pipeline
  stallWire :: Wire (Bit 1) <- makeWireDefault 0
  let stall = scanl (flip reg) (val stallWire) (replicate 4 0)

  -- Accumulator for unioning states
  acc :: Reg VertexState <- makeReg

  -- Fetch-accumulate pipeline
  let fetchAccumulate =
        Do [
          -- Stage 1: fetch next neighbour
          do {
            load neighboursMem (val i); 
            i <== (stall!!0) ? (val i - 1, val i + 1)
          },

          -- Delay
          return (),

          -- Stage 2: fetch state of neighbour
          whenNot (stall!!1 .|. stall!!2) $ do {
            when (val src .==. out' neighboursMem) (stallWire <== 1);
            load stateMem (val active # out' neighboursMem);
          },

          -- Delay
          return (),

          -- Stage 3: update accumulator
          let accNew = val acc .|. out' stateMem in
            whenNot (stall!!3 .|. stall!!4) $ do {
              when (stall!!2) $ do {
                acc <== 0;
                store stateMem (inv (val active) # val src) accNew;
                src <== val src + 1;
                popCountIn <== accNew;
                popCountGo <== 1;
              };
              when (inv (stall!!2)) (acc <== accNew);
            }
        ]

  -- Connect fetch-accumulate pipeline to population-count pipeline
  let (countDone, count) = popCount (val popCountGo) (val popCountIn)

  -- Compile the recipe
  run (val fetchAccumulateGo) fetchAccumulate 

  -- ============
  -- Control path
  -- ============

  -- Number of vertices completed
  completed :: Reg VertexId <- makeReg

  -- Number of requests to make to fetch-accumulate pipeline
  numReqs :: Reg (Bit 32) <- makeReg

  -- Number of responses to receive from population-count pipeline
  numResps :: Reg (Bit 32) <- makeReg

  -- Sum of all shortest paths found so far
  sumPaths :: Reg (Bit 32) <- makeReg

  -- Feed the data path until all vertices reach all other vertices
  let controlPath =
        Seq [
          Do [
            do {
              active <== 0;
              sumPaths <== 0;
            }
          ],
          -- Compute new vertices reachable at each depth
          While (val completed .!=. numVertices) $ Seq [
            Par [
              -- Pulse the fetch-accumulate pipeline numReqs times
              Seq [
                Do [
                  do {
                    numReqs <== numNeighbours + (numVertices .<<. 1);
                    src <== 0;
                    acc <== 0;
                    i <== 0;
                  }
                ],
                While (val numReqs .>. 0) $ Do [
                  do {
                    fetchAccumulateGo <== 1;
                    numReqs <== val numReqs - 1;
                  }
                ]
              ],
              -- Receive numResps responses from population-count pipeline
              Seq [
                Do [
                  do {
                    numResps <== numVertices;
                    completed <== 0;
                  }
                ],
                While (val numResps .>. 0) $ Do [
                  when countDone $ do {
                    numResps <== val numResps - 1;
                    sumPaths <== val sumPaths +
                      zeroExtend (numVertices - count);
                    when (count .==. numVertices) $
                      completed <== val completed + 1;
                  }
                ]
              ]
            ],
            -- Switch buffers
            Do [
              active <== inv (val active)
            ]
          ],
          -- Finished!
          Do [
            display "Sum = n*(n-1)+" (val sumPaths),
            finish
          ]
        ]

  -- Compile the recipe
  done <- run (reg 1 0) controlPath 

  -- Outputs
  output "sum" (val sumPaths)
  output "done" done

  return ()

main :: IO ()
main = do
  netlist makeASPEngine >>= writeVerilog "/tmp/asp.v"
  genHexFiles "n5.edges" "/tmp/"
