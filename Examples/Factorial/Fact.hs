import Blarney
import Blarney.Recipe

fact :: Module ()
fact = do
  -- State
  n   :: Reg (Bit 32) <- makeReg 0
  acc :: Reg (Bit 32) <- makeReg 1

  -- Compute factorial of 10
  let recipe =
        Seq [
          Action do
            n <== 10
        , While (n.val .>. 0) (
            Action do
              n <== n.val - 1
              acc <== acc.val * n.val
          )
        , Action do
            display "fact(10) = %0d" (acc.val)
            finish
        ]

  runOnce recipe

main :: IO ()
main = writeVerilogTop fact "top" "Fact-Verilog/"
