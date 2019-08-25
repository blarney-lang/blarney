import Blarney
import Blarney.RAM

top :: Module ()
top = do
  vecA :: RAM (Bit 8) (Bit 32) <- makeRAM
  vecB :: RAM (Bit 8) (Bit 32) <- makeRAM

  globalTime :: Reg (Bit 32) <- makeReg 0

  i:: Reg (Bit 8) <- makeReg 0

  res :: Wire (Bit 32) <- makeWire dontCare
  let testSeq =
        Seq [
          While (i.val .<. 20) (Do [
          store vecA (i.val) (zeroExtend $ i.val),
          store vecB (i.val) (zeroExtend $ i.val),
          i <== i.val + 1]),
          Do [i <== 0],


          While (i.val .<. 20) (
            Seq [
              Par [Action $ load vecA (i.val), Action $ load vecB (i.val)],
              Action $ res <== (out vecA) .+. (out vecB),
              Action $ i <== i.val + 1
            ]
          )
        ]

  done <- run (reg 1 0) testSeq

  always (when done finish)

  return ()

main :: IO ()
main = writeVerilogTop top "top" "VectorAdd-Test"
