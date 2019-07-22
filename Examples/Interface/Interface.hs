-- Single-element FIFO

import Blarney
import Blarney.Queue
import Blarney.Stream

-- Module that increments each element in a stream
inc :: Stream (Bit 8) -> Module (Stream (Bit 8))
inc xs = do
  -- Output buffer
  buffer <- makeQueue

  always do
    -- Incrementer
    when (xs.canPeek .&. buffer.notFull) $ do
      xs.consume
      enq buffer (xs.peek + 1)

  -- Convert buffer to a stream
  return (buffer.toStream)

-- This function creates an instance of a Verilog module called "inc"
makeIncS :: Stream (Bit 8) -> Module (Stream (Bit 8))
makeIncS = makeInstance "inc"

top :: Module ()
top = do
  -- Counter
  count :: Reg (Bit 8) <- makeReg 0

  -- Input buffer
  buffer <- makeQueue

  -- Create an instance of inc
  out <- makeIncS (buffer.toStream)

  always do
    -- Fill input
    when (buffer.notFull) $ do
      enq buffer (count.val)
      count <== count.val + 1

    -- Consume
    when (out.canPeek) $ do
      out.consume
      display "Got %0d" (out.peek)
      when (out.peek .==. 100) finish

-- Main function
main :: IO ()
main = do
  let dir = "Interface-Verilog/"
  writeVerilogModule inc "inc" dir
  writeVerilogTop top "top" dir
