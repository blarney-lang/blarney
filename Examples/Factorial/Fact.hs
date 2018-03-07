import Blarney

fact :: RTL ()
fact = do
  -- State
  n   :: Reg (Bit 32) <- makeRegInit 0
  acc :: Reg (Bit 32) <- makeRegInit 0

  -- Compute factorial of 10
  let recipe =
        Seq [
          n := 10,
          While (val n .>. 0) $ Par [
              n := val n - 1,
              acc := val acc + val n
          ]
        ]
       
  -- Single cycle pulse
  let pulse = reg 1 0

  -- Trigger factorial recipe on pulse
  done <- run pulse recipe

  -- Display result and terminate simulation
  when done $ do
    display "fact(10) = " (val acc)
    finish

  return ()

main :: IO ()
main = 
  netlist fact >>= writeVerilog "/tmp/fact.v"
