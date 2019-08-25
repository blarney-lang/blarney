import Blarney
import Blarney.RAM

  -- Top level
top :: Module ()
top = do
  -- RAM
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  globalTime :: Reg (Bit 32) <- makeReg 0
  i :: Reg (Bit 8) <- makeReg 0

  always do
    globalTime <== globalTime.val + 1
    display "global time = 0dx" (globalTime.val)

  let testSeq =
        Seq [
          While (i.val .<. 100) (
            Do [store ram (i.val) (1 .<<. i.val), i <== i.val + 1]),
            Do [i <== 0],
            While (i.val .<. 100) (Do [load ram (val i), display "ram[0x%02x]" (val i) " = 0x%024x" (out ram), i <== i.val + 1])
            ]

  done <- run (reg 1 0) testSeq

  always (when done finish)
  return ()

main :: IO ()
main = writeVerilogTop top "top" "RAM-read-write/"
