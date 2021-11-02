import Blarney
import Blarney.Recipe
import System.Environment

data Counter n =
  Counter {
    inc :: Action ()
  , dec :: Action ()
  , val :: Bit n
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
    when (incWire.val .&. inv decWire.val) do
      count <== count.val + 1

    -- Decrement
    when (inv incWire.val .&. decWire.val) do
      count <== count.val - 1

  -- Interface
  let inc = incWire <== 1
  let dec = decWire <== 1
  let val = count.val

  return (Counter inc dec val)

-- Top-level module
top :: Module ()
top = do
  -- 32 bit counter
  counter :: Counter 32 <- makeCounter

  -- Simple test sequence
  let testSeq =
        Seq [
          Action do counter.inc,
          Action do counter.inc,
          Action do
            counter.inc
            counter.dec,
          Action do
            counter.inc,
          Action do
            counter.dec
        ]

  done <- runRecipeOn (reg 1 0) testSeq

  always do
    when done do
      display "Final count = " counter.val
      finish

  return ()

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Counter" "Counter-Verilog/"
