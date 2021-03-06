import Blarney

top :: Module ()
top = do
  -- Create a register
  cycleCount :: Reg (Bit 4) <- makeReg 0
  -- Check for DEBUG plusarg
  let isDebug = testPlusArgs "DEBUG"

  always do
    -- Increment on every cycle
    cycleCount <== cycleCount.val + 1

    -- Display value an every cycle
    display "cycleCount = " (cycleCount.val)

    -- Terminate simulation when count reaches 10
    when (cycleCount.val .==. 10) do
      display "Finished"
      finish

    -- Display "DEBUG" when +DEBUG is passed as a plusargs
    when isDebug do
      display "running with +DEBUG"

--top :: Module ()
--top = do
--  cycleCount :: Reg (Bit 4) <- makeReg 0
--  always do
--    cycleCount <== cycleCount.val + 1
--    display "plop " (cycleCount.val)
--    display (cycleCount.val) " plip"
--    when (cycleCount.val .==. 15) do
--      finish

--top :: Module ()
--top = always do
--  when false finish

main :: IO ()
main = do
  writeVerilogTop top "top" "BasicRTL-Verilog/"
  simulate top
