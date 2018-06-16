import Blarney

top :: RTL ()
top = do
  -- Create a register
  cycleCount :: Reg (Bit 4) <- makeRegInit 0

  -- Increment on every cycle
  cycleCount <== cycleCount.val + 1

  -- Display value an every cycle
  display "cycleCount = " (cycleCount.val)

  -- Terminate simulation when count reaches 10
  when (cycleCount.val .==. 10) $ do
    display "Finished"
    finish

main :: IO ()
main = emitVerilogTop top "top" "/tmp/basic/"
