import Blarney
import Blarney.Queue

makeResettableQueueOfBit8 :: Bit 1 -> Module (Queue (Bit 8))
makeResettableQueueOfBit8 reset = makeInstanceWithReset "makeQueue" reset

top :: Module ()
top = do
  -- Reset wire for queue
  reset :: Wire (Bit 1) <- makeWire 0

  -- Resettable queue
  q <- makeResettableQueueOfBit8 (reset.val)

  -- Test sequence
  let test = 
        Do [
          enq q 5
        , enq q 10
        , display "canDeq = " (q.canDeq)
        , do reset <== true
             display "reset"
        , display "canDeq = " (q.canDeq)
        , enq q 11
        , display "first = %d" (q.first)
        , finish
        ]

  runOnce test

main :: IO ()
main = do
  writeVerilogTop top "top" "Reset-Verilog/"
  writeVerilogModule (makeQueue @(Bit 8)) "makeQueue" "Reset-Verilog/"
