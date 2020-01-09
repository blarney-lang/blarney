import Blarney
import Blarney.Recipe

-- Top-level module
top :: Module ()
top = do
  -- RAM
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  -- Counter
  i :: Reg (Bit 8) <- makeReg 0

  -- Simple test sequence
  let testSeq =
        Seq [
          While (i.val .<. 100) (
            Do [
              store ram (i.val) (1 .<<. i.val),
              i <== i.val + 1
            ]
          ),
          Do [ i <== 0 ],
          While (i.val .<. 100) (
            Do [
              load ram (val i),
              display "ram[0x%02x]" (val i) " = 0x%024x" (out ram),
              i <== val i + 1
            ]
          )
        ]

  done <- run (reg 1 0) testSeq

  always (when done finish)

  return ()

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "RAM-Verilog/"
