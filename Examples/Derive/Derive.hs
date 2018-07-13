-- Single-element FIFO

import Blarney
import GHC.Generics

data MemReq =
  MemReq {
    memOp   :: Bit 1    -- Is it a load or a store request?
  , memAddr :: Bit 32   -- 32-bit address
  , memData :: Bit 32   -- 32-bit data for stores
  }
  deriving (Generic, Bits, FShow)

-- Top-level module
top :: RTL ()
top = do
  let req = MemReq { memOp = 0, memAddr = 100, memData = 0 }
  display "req = " req
  finish

-- Main function
main :: IO ()
main = emitVerilogTop top "top" "Derive-Verilog/"
