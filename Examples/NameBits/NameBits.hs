import Blarney
import System.Environment

top :: Module ()
top = do
  -- Create a register
  cycleCount :: Reg (Bit 4) <- makeReg 0
  let a = nameBits "AAA" (cycleCount.val)
  let b = nameBits "BBB" (cycleCount.val)

  always do
    -- Increment on every cycle
    cycleCount <== b + 1

    -- Display value an every cycle
    display "cycleCount    = " (cycleCount.val)
    display "cycleCount(a) = " a
    display "cycleCount(b) = " b

    -- Terminate simulation when count reaches 10
    when (cycleCount.val .==. 10) do
      display "Finished"
      finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "NameBits" "NameBits-Verilog/"
