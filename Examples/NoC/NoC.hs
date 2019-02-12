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

-- |Fair scheduler. Takes a ('state', 'avail') pair, where the 'avail'
-- list denotes a set of resources that are available.  Returns a
-- ('state', 'choice') pair, where the 'choice' list contains at most
-- one hot bit denoting the chosen resource.  Provided the 'state' is
-- threaded through successive calls to 'sched', resources will be
-- chosen fairly.
sched :: ([Bit 1], [Bit 1]) -> ([Bit 1], [Bit 1])
sched (state, avail) = (state', choice)
  where
    -- Second choice: any available bit
    second = firstHot avail
    -- First choice: an available bit that's not in the history
    first = zipWith (.&.) second (map inv state)
    -- Is the first choice ok?
    firstOk = orList first
    -- Make a choice
    choice = map (firstOk ?) (zip first second)
    -- Compute new state
    state' = map (firstOk ?) (zip (zipWith (.|.) state first) second)

-- |Fair merger
fairMerge :: Bits a => [Bit 1] -> [Stream a] -> Module (Stream a)
fairMerge readys inputs = do
  -- Output buffer (half throughput)
  buffer <- makeShiftQueue 1

  -- Scheduler state (for fairness)
  state <- replicateM (length inputs) (makeReg 0)

  -- Which input streams are available for consumption?
  let avail = [s.canGet .&. r | (s, r) <- zip inputs readys]

  -- Fair scheduler
  let (state', choice) = sched (map val state, avail)
  always do
    zipWithM_ (<==) state state'

  -- Consume
  always do
    forM_ (zip inputs choice) $ \(input, cond) -> do
      when (cond .&. buffer.notFull) do
        get input
        enq buffer (input.value)

  return (buffer.toStream)

-- |One-hot bit list, specifying which route to take
type Route = [Bit 1]

-- |NoC router
router :: Bits a => (a -> Route) -> [Stream a] -> Module [Stream a]
router route inputs =
    sequence [fairMerge ready inputs | ready <- readys]
  where
    readys = transpose [route (inp.value) | inp <- inputs]

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

-- |2D mesh
mesh :: (KnownNat n, Bits a)
     => (a -> Bit n, a -> Bit n)
     -> (Int, Int)
     -> (Stream a -> Module (Stream a))
     -> Module ()
mesh (getX, getY) (lenX, lenY) node = mdo
    outs <- sequence
              [ sequence
                  [ mdo me   <- node (strs.head)
                        strs <- meshRouter (getX, getY)
                                           (fromIntegral x, fromIntegral y)
                                           (me : neighbours (x, y) outs)
                        return strs
                  | x <- [0..lenX-1] ]
              | y <- [0..lenY-1] ]
    return ()
  where
    neighbours coords m =
      [north coords m, south coords m, east coords m, west coords m]
    [n, s, e, w] = [1, 2, 3, 4]
    north (x, y) m = if y < (lenY-1) then m!!(y+1)!!x!!s else nullStream
    south (x, y) m = if y > 0        then m!!(y-1)!!x!!n else nullStream
    east  (x, y) m = if x < (lenX-1) then m!!y!!(x+1)!!w else nullStream
    west  (x, y) m = if x > 0        then m!!y!!(x-1)!!e else nullStream

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
    mesh (destX, destY) (lenX, lenY) node
  where
    node input = do
      buffer <- makeShiftQueue 1
      timer :: Reg (Bit 32) <- makeReg 0
      regX  :: Reg (Bit 8) <- makeReg 0
      regY  :: Reg (Bit 8) <- makeReg 0

      always do
        timer <== timer.val + 1

        when (input.canGet) do
          get input
          display (input.value.destX, input.value.destY)
                  ": t=" (timer.val)

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

  -- Simulate for 1000 cycles
  always do
    timer <== timer.val + 1
    when (timer.val .==. 10000) finish

  -- Flood a 4x4 mesh
  testMesh (4, 4)

-- Main
main :: IO ()
main = writeVerilogTop top "top" "NoC-Verilog/"
