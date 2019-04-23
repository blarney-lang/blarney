import Pebbles
import Blarney
import Blarney.Stream

-- Simulation version
simPebbles :: Module ()
simPebbles = makePebbles True nullStream >> return ()

main :: IO ()
main = do
  writeVerilogTop simPebbles "SimPebbles" "Pebbles-Verilog/"
  writeVerilogModule (makePebbles False) "Pebbles" "Pebbles-Verilog/"
