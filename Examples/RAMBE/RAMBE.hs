import Blarney
import Blarney.Stmt

-- Top-level module
top :: Module ()
top = do
  -- RAM
  ram :: RAMBE 10 4 <- makeRAMBE

  -- Counter
  i :: Reg (Bit 10) <- makeReg 0

  -- Simple test sequence
  runStmt do
    while (i.val .<. 1000) do
      action do
        storeBE ram (i.val) 1 (i.val.zeroExtend)
        i <== i.val + 1
    action do
      i <== 0
    while (i.val .<. 1000) do
      action do
        loadBE ram (i.val)
      action do
        display "ram[" (i.val) "] = " (ram.outBE)
        i <== i.val + 1
    action do
      finish

  return ()

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "RAMBE-Verilog/"
