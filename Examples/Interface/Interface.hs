-- Single-element FIFO

import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.GetPut

-- Module that increments each element in a stream
incS :: Stream (Bit 8) -> RTL (Stream (Bit 8))
incS xs = do
  -- Output buffer
  buffer <- makeQueue

  -- Incrementer
  when (xs.canGet .&. buffer.notFull) $ do
    xs.get
    enq buffer (xs.value + 1)

  -- Convert buffer to a stream
  return (buffer.toStream)

-- This function creates an instance of a Verilog module called "incS"
makeIncS :: Stream (Bit 8) -> RTL (Stream (Bit 8))
makeIncS = makeInstance "incS"

top :: RTL ()
top = do
  -- Counter
  count :: Reg (Bit 8) <- makeRegInit 0

  -- Input buffer
  buffer <- makeQueue

  -- Create an instance of incS
  out <- makeIncS (buffer.toStream)

  -- Fill input
  when (buffer.notFull) $ do
    enq buffer (count.val)
    count <== count.val + 1

  -- Consume
  when (out.canGet) $ do
    out.get
    display "Got " (out.value)
    when (out.value .==. 100) finish
 
-- Main function
main :: IO ()
main = do
  let dir = "Interface-Verilog/"
  emitVerilogModule incS "incS" dir
  emitVerilogTop top "top" dir
