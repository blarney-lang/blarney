import Blarney
import Blarney.RAM

-- Top-level module
top :: RTL ()
top = do
  -- RAM
  ram :: RAM (Bit 8) (Bit 128) <- makeRAM

  -- Counter
  i :: Reg (Bit 8) <- makeRegInit 0

  -- Simple test sequence
  let testSeq =
        Seq [
          While (val i .<. 100) $
            Do [
              store ram (val i) (1 .<<. zeroExtend (val i)),
              i <== val i + 1
            ],
          Do [ i <== 0 ],
          While (val i .<. 100) $
            Do [
              load ram (val i),
              display "ram[" (val i) "] = " (out ram),
              i <== val i + 1
            ]
        ]

  done <- run (reg 1 0) testSeq

  when done finish

  return ()

-- Main function
main :: IO ()
main = netlist top >>= writeVerilog "/tmp/ram.v"
