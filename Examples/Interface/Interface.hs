-- Single-element FIFO

import Blarney
import Blarney.Interface
import Blarney.Queue
import Blarney.Stream
import Blarney.GetPut
import GHC.Generics

-- Module that increments each element in a stream
makeIncS :: Stream (Bit 8) -> RTL (Stream (Bit 8))
makeIncS xs = do
  -- Output buffer
  buffer <- makeQueue

  -- Incrementer
  when (xs.canGet .&. buffer.notFull) $ do
    enq buffer (xs.value + 1)

  -- Convert buffer to a stream
  return (buffer.toStream)
 
instIncS :: Stream (Bit 8) -> RTL (Stream (Bit 8))
instIncS = makeInstance "incS"

top :: RTL ()
top = do
  -- Input buffer
  buffer <- makeQueue

  -- Create an instance
  out <- instIncS (buffer.toStream)

  -- Fill input
  when (buffer.notFull) $ do
    enq buffer 100

  -- Consume
  when (out.canGet) $ do
    out.get
    display "Got " (out.value)
 
-- Main function
main :: IO ()
main = do
  generateVerilog (makeModule makeIncS) "/tmp/inc.v"
  generateVerilog top "/tmp/top.v"

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
