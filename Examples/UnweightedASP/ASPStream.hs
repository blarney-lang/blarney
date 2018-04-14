import Network
import Blarney
import Blarney.RAM
import Blarney.Queue
import Blarney.Stream

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

  -- RAM storing vertex state, implements a double buffer
  -- One half contains current-state, the other contains next-state
  stateMem :: RAM StateAddr VertexState <- makeRAMInit "state.hex"

  -- We treat the two halves of state memory as a double buffer
  -- Which is the active buffer?
  active :: Reg (Bit 1) <- makeRegInit 0

  -- Data path
  -- =========

  -- Determine address for stateMem read stream
  let stateLoadAddr v = active.val # v

  -- Combine the states of each neighbour
  let unionS ins = do
        -- State accumulator
        acc :: Reg VertexState <- makeRegInit 0

        -- Current vertex being processed
        current :: Reg VertexId <- makeRegInit 0

        -- Output buffer
        buffer :: Queue VertexState <- makeQueue

        when (ins.canGet) $ do
          let (v, s) = ins.value
          let accNew = acc.val .|. s
          if v .==. current.val
            then do
              when (buffer.notFull) $ do
                ins.get
                enq buffer accNew
                store stateMem (active.val.inv # v) accNew
                acc <== 0
                let next = current.val + 1
                current <== (next .==. numVertices) ? (0, next) 
            else do
              ins.get
              acc <== acc.val .|. accNew
              

        return (buffer.toStream)

  -- Feed the pipeline with (from, to) range of neighbour ids
  pipeIn :: Queue (NeighbourId, NeighbourId) <- makeQueue

  -- Core pipeline
  reachingVectors <- enumS (toStream pipeIn)
                 >>= loadS neighboursMem id
                 >>= return `o` fmap snd
                 >>= loadS stateMem stateLoadAddr
                 >>= unionS

  -- Consume the reaching vectors at full rate
  when (reachingVectors.canGet) (reachingVectors.get)

  -- Feed the reaching vectors into the population count pipeline
  let (done, pop) = popCount (reachingVectors.canGet) (reachingVectors.value)

  -- Control path
  -- ============

  -- Number of vertices so far that have reach all other vertices
  completed :: Reg VertexId <- makeReg

  -- Number of responses to receive from population-count pipeline
  numResps :: Reg (Bit 32) <- makeReg

  -- Sum of all shortest paths found so far
  sumPaths :: Reg (Bit 32) <- makeReg

  -- Feed the pipeline until all vertices reach all other vertices
  let control =
        Seq [
          Par [ active := 0, sumPaths := 0, completed := 0 ],
          -- Compute new vertices reachable at each depth
          While (completed.val .!=. numVertices) $ Seq [
            Par [ numResps := numVertices, completed := 0 ],
            -- Start the pipeline
            Wait (pipeIn.notFull),
            Do [ enq pipeIn (0, numNeighbours) ],
            -- Receive responses from population-count pipeline
            While (numResps.val .>. 0) $ Do [
              when done $ do
                numResps <== numResps.val - 1
                sumPaths <== sumPaths.val +
                  zeroExtend (numVertices - pop)
                when (pop .==. numVertices) $ do
                  completed <== completed.val + 1
            ],
            -- Switch double-buffer
            active := active.val.inv
          ],
          -- Finished!
          Do [
            display "Sum = n*(n-1)+" (sumPaths.val),
            finish
          ]
        ]

  -- Compile the recipe
  done <- run (reg 1 0) control

  -- Outputs
  output "sum" (val sumPaths)
  output "done" done

  return ()

main :: IO ()
main = do
  netlist makeASPEngine >>= writeCXX "/tmp/asp"
  genHexFiles "n5.edges" "/tmp/asp/"
