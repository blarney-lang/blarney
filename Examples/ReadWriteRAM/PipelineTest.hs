import Blarney
import Blarney.RAM


top :: Module ()
top = do
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  i :: Reg (Bit 8) <- makeReg 0

  let testSeq =
        Seq [
          While (i.val .<. 20) (Do [store ram (i.val) (zeroExtend $ i.val), i <== i.val + 1]),
          Do [i <== 0],

          While (i.val .<. 20) (Do [load ram (val i), display "ram[0x%02x]" (val i) " = 0x%024x" (out ram), i <== i.val + 1])
        ]

  done <- run (reg 1 0) testSeq
  always (when done finish)
  return ()

main :: IO ()
main = writeVerilogTop top "top" "Pipeline-Test"
