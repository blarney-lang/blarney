import Blarney
import Blarney.Stmt

-- Top-level module
top :: Module ()
top = do
  -- RAM
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  -- Counter
  i :: Reg (Bit 8) <- makeReg 0

  -- Simple test sequence
  runStmt do
    while (i.val .<. 100) do
      action do
        store ram (i.val) (1 .<<. i.val)
        i <== i.val + 1
    action do
      i <== 0
    while (i.val .<. 100) do
      action do
        load ram (i.val)
      action do
        display "ram[" (i.val) "] = 0x" (formatHex 24 (ram.out))
        i <== i.val + 1
    action do
      finish

  return ()

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "RAM-Verilog/"
