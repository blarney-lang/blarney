import Blarney
import Blarney.Queue
import System.Environment

top :: Module ()
top = do
  -- Create a list of 4 queues
  queues :: [Queue (Bit 8)] <- replicateM 4 makeQueue

  -- Cycle count
  cycleCount :: Reg (Bit 8) <- makeReg 0

  always do
    cycleCount <== cycleCount.val + 1

    -- Create index using lower 2 bits of cycle count
    let ind :: Bit 2 = truncate cycleCount.val

    -- Pick a queue using the index
    let q = queues ! ind

    -- Write to queue
    when q.notFull do
      q.enq (zeroExtend ind)

    -- Consume from queue
    when q.canDeq do
      q.deq
      display cycleCount.val ": " q.first

    -- Terminate simulation when count reaches 16
    when (cycleCount.val .==. 16) do
      display "Finished"
      finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Lookup" "Lookup-Verilog/"
