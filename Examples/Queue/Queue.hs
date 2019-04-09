import Blarney
import Blarney.Queue

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
      display "Got %0d" (queue.first)
      when (queue.first .>. 100) finish

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "Queue-Verilog"
