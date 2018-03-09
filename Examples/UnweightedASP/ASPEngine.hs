import Blarney
import Network

-- This is a general ASP-finding engine,
-- but for the purposes of testing, let's define:
#define numVertices 15
#define numNeighbours 57
#define LogMaxNeighbours 16
#define LogMaxVertices 4
#define LogMaxVerticesPlusOne 5
#define MaxVertices 32

-- For a 4096 node engine:
-- #define LogMaxVertices 12
-- #define LogMaxVerticesPlusOne 13
-- #define MaxVertices 128

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
    count   = tree1 add [zeroExtend (bit vec i) | i <- [0..2^n-1]]
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
  stall :: Wire (Bit 1) <- makeWireDefault 0
  let stall' = reg 0 (val stall)
  let stall'' = reg 0 stall'

  -- Accumulator for unioning states
  acc :: Reg VertexState <- makeReg

  -- Fetch-accumulate pipeline
  let fetchAccumulate =
        Do [
          -- Stage 1: fetch next neighbour
          do {
            load neighboursMem (val i); 
            whenNot (val stall) (i <== val i + 1);
          },

          -- Stage 2: fetch state of neighbour
          whenNot stall' $ do {
            when (val src .==. out neighboursMem) (stall <== 1);
            load stateMem (val active # out neighboursMem);
          },

          -- Stage 3: update accumulator
          let accNew = val acc .|. out stateMem in
            whenNot stall'' $ do {
              when stall' $ do {
                acc <== 0;
                store stateMem (inv (val active) # val src) accNew;
                src <== val src + 1;
                popCountIn <== val acc .&. inv (out stateMem);
                popCountGo <== 1;
              };
              when (inv stall') (acc <== accNew);
            }
        ]

  -- Connect fetch-accumulate pipeline to population-count pipeline
  let (countDone, count) = popCount (val popCountGo) (val popCountIn)

  -- Compile the recipe
  run (val fetchAccumulateGo) fetchAccumulate 

  -- ============
  -- Control path
  -- ============

  -- Current depth of memoised iterative deepening
  depth :: Reg VertexId <- makeReg

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
              depth <== 1;
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
                    numReqs <== numNeighbours + numVertices;
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
                    sumPaths <== val sumPaths + zeroExtend count
                                              * zeroExtend (val depth);
                    when (count .==. 0) (completed <== val completed + 1);
                  }
                ]
              ]
            ],
            -- Increment depth and switch buffers
            Do [
              do {
                depth <== val depth + 1;
                active <== inv (val active);
              }
            ]
          ],
          -- Finished!
          Do [
            display "Sum = " (val sumPaths),
            finish
          ]
        ]

  -- Compile the recipe
  run (reg 1 0) controlPath 

  return ()

main :: IO ()
main = do
  netlist makeASPEngine >>= writeCXX "/tmp/asp/"
  genHexFiles "n1.edges" "/tmp/asp/"
