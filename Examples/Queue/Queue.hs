import Blarney
import Blarney.Queue

-- Test some queue properties
data QueueCmd a =
  QueueCmd {
    cmdEnq :: a -> Action ()
  , cmdDeq :: Action ()
  }
  deriving (Generic, Interface)
cosim :: (Num a, Bits a) => Queue a -> Queue a -> Module (QueueCmd a)
cosim q1 q2 = do
  always do
    assert (q1.notFull .==. q2.notFull) "prop_notFull"
           "q1 and q2 notFull signal should be equal"
    assert (q1.notEmpty .==. q2.notEmpty) "prop_notEmpty"
           "q1 and q2 notEmtpy signal should be equal"
    assert (q1.canDeq .&. q2.canDeq .==>. q1.first === q2.first)
           "prop_First"
           "q1 and q2 first elements should be equal"
  return
    QueueCmd {
      cmdEnq = \x ->
        when (q1.notFull .&. q2.notFull) do
          enq q1 x
          enq q2 x
    , cmdDeq =
        when (q1.canDeq .&. q2.canDeq) do
          q1.deq
          q2.deq
    }
prop_queueEquiv :: KnownNat n => Module (QueueCmd (Bit n))
prop_queueEquiv = do
  q <- makeQueue
  sq <- makeShiftQueue 2
  cosim q sq

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
  writeVerilogTop top "top" "Queue-Verilog"
  writeVerilogModule (prop_queueEquiv @1) "prop_queueEquiv" "Queue-Verilog"
  writeSMT2Script (prop_queueEquiv @1) "prop_queueEquiv" "Queue-SMT2"
