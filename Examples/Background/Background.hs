{-# LANGUAGE MultiWayIf #-}

import Blarney
import Blarney.Recipe

import System.Environment

block :: [Action ()] -> Recipe
block acts = Seq (map Action acts)

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
            block [
              display "storing " (i.val),
              store vecA (i.val) (i.val.zeroExtend),
              store vecB (i.val) (i.val.zeroExtend),
              i <== i.val + 1
            ]
          ),

          block [
            i <== 0,
            display "starting at " (globalTime.val)
          ],

          While (i.val .<. 20) (
            block [
              do load vecA (i.val)
                 load vecB (i.val)
                 display "loading un-pipelined at time " (globalTime.val),
              do store vecCNoPipe (i.val) (vecA.out + vecB.out)
                 i <== i.val + 1
            ]
          ),

          block [
            display "un-pipelined loop ending at " (globalTime.val),
            i <== 0,
            display "C values after un-pipelined add"
          ],

          While (i.val .<. 20) (
            block [
              load vecCNoPipe (i.val),
              display "C[" (i.val) "] = " (out vecCNoPipe),
              i <== i.val + 1
            ]
          ),

          block [
            i <== 0,
            display "Clearing C values"
          ],

          While (i.val .<. 20) (
            block [
              store vecCPipe (i.val) 0,
              i <== i.val + 1
            ]
          ),


          block [
            i <== 0,
            display "Running pipelined vector add...",
            display "starting pipelined loop at " (globalTime.val)
          ],

          While (i.val .<. 20) (
           Seq [
             Background (
               Seq [
                 Action do
                   load vecA (i.val)
                   load vecB (i.val)
                   i0 <== i.val
                   display "load values at time " (globalTime.val),
                 Action do
                   store vecCPipe (i0.val) (vecA.out + vecB.out)
               ]
             ),
             block [i <== i.val + 1]
            ]
          ),

          block [
            display "ending pipelined loop at " (globalTime.val),
            i <== 0,
            display "After pipelined add..."
          ],

          While (i.val .<. 20) (
            block [
              load vecCPipe (i.val),
              display "C[" (i.val) "] = " (out vecCPipe),
              i <== i.val + 1
            ]
          )

        ]

  always do
    globalTime <== globalTime.val + 1

  done <- runRecipeOn (reg 1 0) testSeq

  always (when done finish)

  return ()

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Background" "Background-Verilog/"
