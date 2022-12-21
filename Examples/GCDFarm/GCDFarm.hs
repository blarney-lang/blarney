import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import System.Environment

-- Server for computing GCDs using Euclid's algorithm
makeGCDServer :: (Bits a, Cmp a, Num a) => Stream (a, a) -> Module (Stream a)
makeGCDServer reqs = do
  -- State
  answers <- makeQueue
  a <- makeReg dontCare
  b <- makeReg dontCare

  runStmt do
    while true do
      -- Consume request
      wait reqs.canPeek
      action do
        let (numA, numB) = reqs.peek
        a <== numA
        b <== numB
        reqs.consume
      -- Compute GCD
      while (a.val .!=. b.val) do
        action do
          if a.val .>. b.val
            then a <== a.val - b.val
            else b <== b.val - a.val
      -- Produce answer
      wait answers.notFull
      action do
        answers.enq a.val

  return (toStream answers)
      
-- Model of a server using functions over streams
type Server req resp = Stream req -> Module (Stream resp)

makeServerFarm :: Interface resp => Int -> Server req resp -> Server req resp
makeServerFarm n server reqs =
  splitStream n reqs >>= mapM server >>= mergeStreams

splitStream :: Int -> Stream a -> Module [Stream a]
splitStream n s = do
  -- Which output stream to feed next?
  next :: [Reg (Bit 1)] <-
    mapM makeReg ([true] ++ replicate (n-1) false)

  -- When an output stream is consumed, move to the next one
  return
    [ s {
        canPeek = s.canPeek .&&. active.val
      , consume = s.consume >> rotate next
      }
    | active <- next ]

rotate :: Bits a => [Reg a] -> Action ()
rotate xs = zipWithM_ (<==) xs (drop 1 vals ++ take 1 vals)
  where vals = map (.val) xs

mergeStreams :: Interface a => [Stream a] -> Module (Stream a)
mergeStreams ss = do
  -- Which input stream to consume next?
  next :: [Reg (Bit 1)] <-
    mapM makeReg ([true] ++ replicate (length ss - 1) false)

  -- Select stream using general indexing operator
  let s = ss ! OneHotList (map (.val) next)

  -- When output is consumed, move to the next input stream
  return
    s { consume = s.consume >> rotate next }

-- Top-level test bench
top :: Module ()
top = do
  -- Input buffer of size 32
  inputs <- makeSizedQueue 5

  -- Instantiate farm of 4 servers
  outputs <- makeServerFarm 4 makeGCDServer (toStream inputs)

  -- Timer
  timer :: Reg (Bit 32) <- makeReg 0
  always do timer <== timer.val + 1

  runStmt do
    wait inputs.notFull
    action do inputs.enq (693 :: Bit 32, 700)

    wait inputs.notFull
    action do inputs.enq (81, 27)

    wait inputs.notFull
    action do inputs.enq (500, 495)

    replicateM 3 do
      wait outputs.canPeek
      action do
        display outputs.peek
        outputs.consume

    action do
      display "Finished at time " timer.val
      finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "GCDFarm" "GCDFarm-Verilog/"
