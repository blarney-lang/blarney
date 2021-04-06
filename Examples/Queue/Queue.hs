import Blarney
import Blarney.Queue
import System.Environment

-- Top-level module
top :: Module ()
top = do
  -- Queue
  queue :: Queue (Bit 32) <- makeSizedQueue 3

  -- Counter
  count :: Reg (Bit 32) <- makeReg 0

  always do
    count <== count.val + 1

    -- Feed queue
    when (queue.notFull) do
      enq queue (count.val)

    -- Consume queue
    when (queue.canDeq .&. (count.val .>. 50)) do
      queue.deq
      display "Got " (queue.first)
      when (queue.first .>. 100) finish

-- Main function
main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Queue" "Queue-Verilog/"
