import Blarney
import Blarney.RAM

parA m = Par $ map Action m
act = Action

top :: Module ()
top = do
  vecA :: RAM (Bit 8) (Bit 32) <- makeRAM
  vecB :: RAM (Bit 8) (Bit 32) <- makeRAM

  vecCNoPipe :: RAM (Bit 8) (Bit 32) <-makeRAM
  vecCPipe :: RAM (Bit 8) (Bit 32) <-makeRAM
  
  globalTime :: Reg (Bit 32) <- makeReg 0

  i :: Reg (Bit 8) <- makeReg 0
  i0 :: Reg (Bit 8) <- makeReg dontCare
  i1 :: Reg (Bit 8) <- makeReg dontCare

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
              parA [load vecA (i.val), load vecB (i.val), display "loading un-pipelined at time %02d" (globalTime.val)],
              parA [store vecCNoPipe (i.val) ((out vecA) + (out vecB)), i <== i.val + 1]
            ]
          ),
  
          Do [i <== 0],
          act $ display "C values after un-pipelined add",

          While (i.val .<. 20) (
            Do [
              load vecCNoPipe (i.val),
              display "C[%02d]" (i.val) " = %02d" (out vecCNoPipe),
              i <== i.val + 1
            ]
          ),

          Do [i <== 0],
          act $ display "Clearing C values",

          While (i.val .<. 20) (
            Do [
              store vecCPipe (i.val) 0,
              i <== i.val + 1
            ]
          ),

        
          Do [i <== 0],
          act $ display "Running pipelined vector add...",

          While (i.val .<. 20) (
           Seq [
             Launch (
               Seq [
                parA [load vecA (i.val), load vecB (i.val), i0 <== i.val, display "load values at time %02d" (globalTime.val)],
                act $ store vecCPipe (i0.val) ((out vecA) + (out vecB))
               ]
             ),
             act $ i <== i.val + 1
            ]
          ),


          Do [i <== 0],
          act $ display "After pipelined add...",

          While (i.val .<. 20) (
            Do [
              load vecCPipe (i.val),
              display "C[%02d]" (i.val) " = %02d" (out vecCPipe),
              i <== i.val + 1
            ]
          )

        ]

  always do
    globalTime <== globalTime.val + 1
  
  done <- run (reg 1 0) testSeq

  always (when done finish)

  return ()

main :: IO ()
main = writeVerilogTop top "top" "VectorAdd-Test"
