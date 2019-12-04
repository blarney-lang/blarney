import Blarney
import Blarney.Queue
import Blarney.Stream
import Data.List (transpose)
import Control.Monad (forM_, zipWithM_, replicateM)

-- |Isolate first hot bit
firstHot :: [Bit 1] -> [Bit 1]
firstHot = first 1
  where
    first ok [] = []
    first ok (x:xs) = (x .&. ok) : first (inv x .&. ok) xs

-- |Conditional merger
condMerge :: Bits a => [Bit 1] -> [Stream a] -> Module (Stream a)
condMerge readys inputs = do
  -- Output buffer (half throughput)
  buffer <- makeShiftQueue 1

  -- Which input streams are available for consumption?
  let avail = [s.canPeek .&. r | (s, r) <- zip inputs readys]
  let choice = firstHot avail

  -- Consume
  always do
    forM_ (zip inputs choice) $ \(input, cond) -> do
      when (cond .&. buffer.notFull) do
        consume input
        enq buffer (input.peek)

  return (buffer.toStream)

-- |Merger
merge :: Bits a => [Stream a] -> Module (Stream a)
merge inputs = condMerge (replicate n 1) inputs
  where n = length inputs

-- |Splitter
fork :: Bits a => Int -> Stream a -> Module [Stream a]
fork 1 inp = return [inp]
fork n inp = do
  -- Output buffers (half throughput)
  buffers <- replicateM n (makeShiftQueue 1)

  -- Which output streams are available to fill?
  let avail = [b.notFull | b <- buffers]
  let choice = firstHot avail

  -- Fill
  always do
    forM_ (zip buffers choice) $ \(buffer, cond) -> do
      when (cond .&. inp.canPeek) do
        consume inp
        enq buffer (inp.peek)

  return (map toStream buffers)

-- |One-hot bit list, specifying which route to take
type Route = [Bit 1]

-- |NoC router
router :: Bits a => (a -> Route) -> [Stream a] -> Module [Stream a]
router route inputs =
    sequence [condMerge ready inputs | ready <- readys]
  where
    readys = transpose [route (inp.peek) | inp <- inputs]

-- |Mesh router
meshRouter :: Bits a
           => (a -> Bit n, a -> Bit n)
           -> (Bit n, Bit n)
           -> [Stream a]
           -> Module [Stream a]
meshRouter (getX, getY) (x, y) inputs = router route inputs
  where
    mine  pkt = (pkt.getX .==. x) .&. (pkt.getY .==. y)
    north pkt = pkt.getY .>. y
    south pkt = pkt.getY .<. y
    east  pkt = (pkt.getY .==. y) .&. (pkt.getX .>. x)
    west  pkt = (pkt.getY .==. y) .&. (pkt.getX .<. x)
    route pkt = [mine pkt, north pkt, south pkt, east pkt, west pkt]

-- |Multi-channel 2D mesh
mesh :: (KnownNat n, Bits a)
     => Int
     -> (a -> Bit n, a -> Bit n)
     -> (Int, Int)
     -> ((Int, Int) -> Stream a -> Module (Stream a))
     -> Module ()
mesh chans (getX, getY) (lenX, lenY) node = mdo
    outs <- sequence
              [ sequence
                  [ mdo nodeIn <- merge (map head strs)
                        nodeOut <- node (x, y) nodeIn
                        nodeOuts <- fork chans nodeOut
                        strs <- sequence
                          [ meshRouter (getX, getY)
                                       (fromIntegral x, fromIntegral y)
                                       (nodeOuts!!c : neighbours c (x, y) outs)
                          | c <- [0..chans-1]
                          ]
                        return strs
                  | x <- [0..lenX-1] ]
              | y <- [0..lenY-1] ]
    return ()
  where
    neighbours c coords m =
      [north c coords m, south c coords m, east c coords m, west c coords m]
    [n, s, e, w] = [1, 2, 3, 4]
    north c (x, y) m = if y < (lenY-1) then m!!(y+1)!!x!!c!!s else nullStream
    south c (x, y) m = if y > 0        then m!!(y-1)!!x!!c!!n else nullStream
    east  c (x, y) m = if x < (lenX-1) then m!!y!!(x+1)!!c!!w else nullStream
    west  c (x, y) m = if x > 0        then m!!y!!(x-1)!!c!!e else nullStream

-- |Example 2D packet format
data MeshPkt =
  MeshPkt {
    payload :: Bit 8
  , destX   :: Bit 8
  , destY   :: Bit 8
  } deriving (Generic, Bits, FShow)

-- |Test bench
testMesh :: (Int, Int) -> Module ()
testMesh (lenX, lenY) =
    mesh 1 (destX, destY) (lenX, lenY) node
  where
    node (x, y) input = do
      buffer <- makeShiftQueue 1
      timer :: Reg (Bit 32) <- makeReg 0
      regX  :: Reg (Bit 8) <- makeReg 0
      regY  :: Reg (Bit 8) <- makeReg 0

      always do
        timer <== timer.val + 1

        when (input.canPeek) do
          consume input
          display (x,y) ": dest=(%0d,%0d)"
                  (input.peek.destX) (input.peek.destY)
                  " t=%0d" (timer.val)

        when (buffer.notFull) do
          let pkt = MeshPkt { payload = 0,
                              destX   = regX.val,
                              destY   = regY.val }
          enq buffer pkt
          if regX.val .==. fromIntegral (lenX-1)
            then do
              regX <== 0
              if (regY.val .==. fromIntegral (lenY-1))
                then regY <== 0
                else regY <== regY.val + 1
            else do
              regX <== regX.val + 1

      return (buffer.toStream)

-- Top level
top :: Module ()
top = do
  -- Timer
  timer :: Reg (Bit 32) <- makeReg 0

  -- Flood a 4x4 mesh
  testMesh (4, 4)

  -- Simulate for 1000 cycles
  always do
    timer <== timer.val + 1
    when (timer.val .==. 10000) finish

-- Main
main :: IO ()
main = writeVerilogTop top "top" "NoC-Verilog/"
