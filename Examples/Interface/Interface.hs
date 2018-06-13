-- Single-element FIFO

import Blarney
import Blarney.Interface
import Blarney.Queue
import Blarney.Stream
import Blarney.GetPut
import GHC.Generics

-- Module that increments each element in a stream
incS :: Stream (Bit 8) -> RTL (Stream (Bit 8))
incS xs = do
  -- Output buffer
  buffer <- makeQueue

  -- Incrementer
  when (xs.canGet .&. buffer.notFull) $ do
    enq buffer (xs.value + 1)

  -- Convert buffer to a stream
  return (buffer.toStream)
  
-- Main function
main :: IO ()
main = generateVerilog (makeModule incS) "/tmp/interface.v"

{-

This example demonstrates interfaces containing functions, i.e. the
"put" method in the "Put" interface.

-- Top-level module
top :: Put (Bit 8) -> RTL (Put (Bit 8))
top inp = do
  display "Hello world"
  finish
  return $ Put { canPut = inp.canPut, put = \x -> put inp x }

-}
