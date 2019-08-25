import Blarney
import Blarney.RAM

parA m = Par $ map Action m
act = Action

top :: Module ()
top = do
  vecA :: RAM (Bit 8) (Bit 32) <- makeRAM
  vecB :: RAM (Bit 8) (Bit 32) <- makeRAM

  vecC :: RAM (Bit 8) (Bit 32) <-makeRAM

  globalTime :: Reg (Bit 32) <- makeReg 0

  i:: Reg (Bit 8) <- makeReg 0

  res :: Reg (Bit 32) <- makeReg dontCare
  let testSeq =
        Seq [
          While (i.val .<. 20) (Do [
          display "storing %02d" (i.val),
          store vecA (i.val) (zeroExtend $ i.val),
          store vecB (i.val) (zeroExtend $ i.val),
          i <== i.val + 1]),

          Do [i <== 0],


          While (i.val .<. 20) (
            Seq [
              parA [load vecA (i.val), load vecB (i.val)],

              Par [act $ display "a val = %02d" (out vecA), act $ display "b val = %02d" (out vecB), act $ res <== (out vecA) +  (out vecB)],
              act $ display "res = %02d" (res.val ), 
              act $ store vecC (i.val) (val res),
              act $ display "storing %02d" (res.val),
              act $ i <== i.val + 1
            ]
          ),
  
          Do [i <== 0],
          act $ display "C values after un-pipelined add",

          While (i.val .<. 20) (
            Do [
              load vecC (i.val),
              display "C[%02d]" (i.val) " = %02d" (out vecC),
              i <== i.val + 1
            ]
          )

        ]

  done <- run (reg 1 0) testSeq

  always (when done finish)

  return ()

main :: IO ()
main = writeVerilogTop top "top" "VectorAdd-Test"
