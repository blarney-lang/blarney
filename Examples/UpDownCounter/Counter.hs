import Blarney

data Counter n =
  Counter {
    inc   :: RTL ()
  , dec   :: RTL ()
  , value :: Bit n
  }

makeCounter :: KnownNat n => RTL (Counter n)
makeCounter = do
  -- State
  count :: Reg (Bit n) <- makeRegInit 0

  -- Wires
  incWire :: Wire (Bit 1) <- makeWireDefault 0
  decWire :: Wire (Bit 1) <- makeWireDefault 0

  -- Increment
  when (val incWire .&. inv (val decWire)) $ do
    count <== val count + 1

  -- Decrement
  when (inv (val incWire) .&. val decWire) $ do
    count <== val count - 1

  -- Interface
  let inc   = incWire <== 1
  let dec   = decWire <== 1
  let value = val count

  return (Counter inc dec value)

-- Top-level module
top :: RTL ()
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

  when done $ do
    display "Final count = " (value counter)
    finish

  return ()

main :: IO ()
main = netlist top >>= writeNetlist "/tmp/counter.net"
