import Blarney
import Blarney.Option
import System.Environment

testModule :: Module (Option (Bit 32), Bit 32)
testModule = do
  testReg :: Reg (Bit 32) <- makeReg 0
  always do testReg <== testReg.val + 1
  let optVal = if testReg.val .%. 5 === 0 then some (testReg.val) else none
  return (optVal ,testReg.val)

top :: Module ()
top = do
  -- Create a testModule
  (opt, noopt) <- testModule
  cnt :: Reg (Bit 32) <- makeReg 0
  always do
    -- Display values
    display "opt = " opt ", noopt = " noopt
    display "cnt = " (cnt.val)
    -- update counter
    when (isSome opt) do cnt <== cnt.val + 1
    -- Terminate simulation
    when (cnt.val .==. 5) do
      display "Finished"
      finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> do
       writeVerilogModule testModule "testModule" "OptionExample-Verilog/"
       writeVerilogTop top "OptionExample" "OptionExample-Verilog/"
