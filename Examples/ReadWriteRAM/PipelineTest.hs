import Blarney
import Blarney.RAM


top :: Module ()
top = do
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  globalTime :: Reg (Bit 32) <- makeReg 0
  i :: Reg (Bit 8) <- makeReg 0

  -- What Id like:
  -- WhileP (i.val .<. 20) (Do [NB $ store ram (i.val), NB $ i <== i.val + 1, iterate])
  -- Q: When iterate is placed somewhere does that mean "iterate on the next cycle? or does it mean iterate in the current cycle?"
  -- A: In the current cycle, but when would you want to do that?

  -- WhileP (i.val .<. 20) (NB [store ram (i.val), Do [i <== i.val + 1, iterate]]) 
  -- In this framing the datapath operations and the index calculator are different

  -- WhileP (i.val .<. 20) (1) (Do [store ram (i.val), i <== i.val + 1])
  -- The problem with this implementation is that i is not set until after the store, but it
  -- needs to be set in the first cycle
  let testSeq =
        Seq [
          While (i.val .<. 20) (Do [store ram (i.val) (zeroExtend $ i.val), i <== i.val + 1]),
          Do [i <== 0],

          While (i.val .<. 20) (Do [load ram (val i), display "ram[0x%02x]" (val i) " = 0x%024x" (out ram), i <== i.val + 1])
        ]

  done <- run (reg 1 0) testSeq
  always (when done finish)

  always do
    globalTime <== globalTime.val + 1
    display "global time = %02d" (val globalTime)

  return ()

main :: IO ()
main = writeVerilogTop top "top" "Pipeline-Test"
