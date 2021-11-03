import Blarney
import Blarney.Stmt
import System.Environment

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
        ram.store i.val (1 .<<. i.val)
        i <== i.val + 1
    action do
      i <== 0
    while (i.val .<. 100) do
      action do
        ram.load i.val
      action do
        display "ram[" i.val "] = 0x" (formatHex 32 ram.out)
        i <== i.val + 1
    action do
      finish

  return ()

-- Main function
main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "RAM" "RAM-Verilog/"
