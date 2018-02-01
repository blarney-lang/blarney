import Blarney

fact :: RTL ()
fact = do
  -- State
  n :: Reg (Bit 32) <- makeReg 8
  acc :: Reg (Bit 32) <- makeReg 0

  -- Recipe to compute factorial
  let recipe =
        While (val n .>. 0) $ Do [
          do n <== val n - 1
             acc <== val acc + val n
        ]

  -- Single cycle pulse
  let pulse = reg 1 0

  -- Trigger factorial recipe on pulse
  done <- run pulse recipe

  -- Display result when recipe done
  when done $ do
    display "fact(8) = " (val acc)
    finish

main :: IO ()
main = 
  netlist fact >>= writeVerilog "/tmp/fact.v"
