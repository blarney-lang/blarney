import Blarney
import Blarney.Recipe

data Counter n =
  Counter {
    inc   :: Action ()
  , dec   :: Action ()
  , value :: Bit n
  }

makeCounter :: KnownNat n => Module (Counter n)
makeCounter = do
  -- State
  count :: Reg (Bit n) <- makeReg 0

  -- Wires
  incWire :: Wire (Bit 1) <- makeWire 0
  decWire :: Wire (Bit 1) <- makeWire 0

  always do
    -- Increment
    when (incWire.val .&. decWire.val.inv) do
      count <== count.val + 1

    -- Decrement
    when (incWire.val.inv .&. decWire.val) do
      count <== count.val - 1

  -- Interface
  let inc   = incWire <== 1
  let dec   = decWire <== 1
  let value = val count

  return (Counter inc dec value)

-- Top-level module
top :: Module ()
top = do
  -- 32 bit counter
  counter :: Counter 32 <- makeCounter

  -- Simple test sequence
  let testSeq =
        Do [
          inc counter,
          inc counter,
          do { inc counter; dec counter },
          inc counter,
          dec counter
        ]

  done <- run (reg 1 0) testSeq

  always do
    when done do
      display "Final count = %0d" (value counter)
      finish

  return ()

main :: IO ()
main = writeVerilogTop top "top" "Counter-Verilog/"
