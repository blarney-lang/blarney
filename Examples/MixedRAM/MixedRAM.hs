import Blarney
import Blarney.RAM

-- Top-level module
top :: Module ()
top = do
  -- Mixed-width RAM
  (ram1, ram2) :: (RAM (Bit 9) (Bit 32), RAM (Bit 8) (Bit 64))
               <- makeTrueDualRAMMixed

  -- Counter
  i :: Reg (Bit 8) <- makeReg 0

  -- Simple test sequence
  let testSeq =
        Seq [
          Do [store ram1 0 0xaa]
        , Do [store ram1 1 0x11]
        , Do [load ram2 0]
        , Do [display "0x" (ram2.out)]
        ]

  done <- run (reg 1 0) testSeq

  always (when done finish)

  return ()

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "MixedRAM-Verilog/"
