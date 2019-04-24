import Pebbles
import Blarney
import Blarney.Stream

-- Simulation version
simPebbles :: Module ()
simPebbles = do
  uartOut <- makePebbles True nullStream
  always do
    when (uartOut.canGet) do
      display "%c" (uartOut.value)
      uartOut.get

main :: IO ()
main = do
  writeVerilogTop simPebbles "SimPebbles" "Pebbles-Verilog/"
  writeVerilogModule (makePebbles False) "Pebbles" "Pebbles-Verilog/"
