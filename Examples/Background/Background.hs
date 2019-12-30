import Blarney
import Blarney.Recipe

top :: Module ()
top = do
  vecA :: RAM (Bit 8) (Bit 32) <- makeRAM
  vecB :: RAM (Bit 8) (Bit 32) <- makeRAM

  vecCNoPipe :: RAM (Bit 8) (Bit 32) <-makeRAM
  vecCPipe :: RAM (Bit 8) (Bit 32) <-makeRAM

  globalTime :: Reg (Bit 32) <- makeReg 0

  i :: Reg (Bit 8) <- makeReg 0
  i0 :: Reg (Bit 8) <- makeReg dontCare

  res :: Reg (Bit 32) <- makeReg dontCare
  let testSeq =
        Seq [
          While (i.val .<. 20) (
            Do [
              display "storing %02d" (i.val),
              store vecA (i.val) (i.val.zeroExtend),
              store vecB (i.val) (i.val.zeroExtend),
              i <== i.val + 1
            ]
          ),

          Do [
            i <== 0,
            display "starting at %02d" (globalTime.val)
          ],

          While (i.val .<. 20) (
            Do [
              do load vecA (i.val)
                 load vecB (i.val)
                 display "loading un-pipelined at time %02d" (globalTime.val),
              do store vecCNoPipe (i.val) (vecA.out + vecB.out)
                 i <== i.val + 1
            ]
          ),

          Do [
            display "un-pipelined loop ending at %d" (globalTime.val),
            i <== 0,
            display "C values after un-pipelined add"
          ],

          While (i.val .<. 20) (
            Do [
              load vecCNoPipe (i.val),
              display "C[%02d]" (i.val) " = %02d" (out vecCNoPipe),
              i <== i.val + 1
            ]
          ),

          Do [
            i <== 0,
            display "Clearing C values"
          ],

          While (i.val .<. 20) (
            Do [
              store vecCPipe (i.val) 0,
              i <== i.val + 1
            ]
          ),


          Do [
            i <== 0,
            display "Running pipelined vector add...",
            display "starting pipelined loop at %d" (globalTime.val)
          ],

          While (i.val .<. 20) (
           Seq [
             Background (
               Seq [
                 Action do
                   load vecA (i.val)
                   load vecB (i.val)
                   i0 <== i.val
                   display "load values at time %02d" (globalTime.val),
                 Action do
                   store vecCPipe (i0.val) (vecA.out + vecB.out)
               ]
             ),
             Do [i <== i.val + 1]
            ]
          ),

          Do [
            display "ending pipelined loop at %d" (globalTime.val),
            i <== 0,
            display "After pipelined add..."
          ],

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
main = writeVerilogTop top "top" "Background-Verilog"
