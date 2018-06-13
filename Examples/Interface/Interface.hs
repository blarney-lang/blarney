-- Single-element FIFO

import Blarney
import Blarney.Interface
import Blarney.GetPut
import GHC.Generics

-- Top-level module
top :: Put (Bit 8) -> RTL (Put (Bit 8))
top inp = do
  display "Hello world"
  finish
  return $ Put { canPut = inp.canPut, put = \x -> display x }

-- Main function
main :: IO ()
main = generateVerilog (makeModule top) "/tmp/interface.v"
