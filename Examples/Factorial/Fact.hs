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
          While (n.val .>. 0) $ Par [
              n := n.val - 1,
              acc := acc.val + n.val
          ]
        ]
       
  -- Single cycle pulse
  let pulse = reg 1 0

  -- Trigger factorial recipe on pulse
  done <- run pulse recipe

  -- Display result and terminate simulation
  when done $ do
    display "fact(10) = " (acc.val)
    finish

  return ()

main :: IO ()
main = 
  netlist fact >>= writeVerilog "/tmp/fact.v"
