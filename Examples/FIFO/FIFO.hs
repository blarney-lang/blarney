import Blarney
import System.Environment

-- Single-element FIFO

-- FIFO interface
data FIFO a =
  FIFO {
    notFull  :: Bit 1
  , notEmpty :: Bit 1
  , enq      :: a -> Action ()
  , deq      :: Action ()
  , first    :: a
  }

-- FIFO module (simple one-element FIFO)
makeFIFO :: Bits a => Module (FIFO a)
makeFIFO = do
  -- Register holding the one element
  reg :: Reg a <- makeReg dontCare

  -- Register defining whether or not FIFO is full
  full :: Reg (Bit 1) <- makeReg 0

  -- Methods
  let notFull = full.val .==. 0

  let notEmpty = full.val .==. 1

  let enq a = do
        reg <== a
        full <== 1

  let deq = full <== 0

  let first = val reg

  -- Return interface
  return (FIFO notFull notEmpty enq deq first)

-- Top-level module
top :: Module ()
top = do
  -- Counter
  timer :: Reg (Bit 8) <- makeReg 0

  -- Instantiate a FIFO
  fifo :: FIFO (Bit 8) <- makeFIFO

  always do
    timer <== timer.val + 1

    -- Writer side
    when (fifo.notFull) $ do
      fifo.enq timer.val
      display "Enqueued " timer.val

    -- Reader side
    when (fifo.notEmpty) $ do
      fifo.deq
      display "Dequeued " fifo.first

    -- Terminate after 100 cycles
    when (timer.val .==. 100) finish

-- Main function
main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "FIFO" "FIFO-Verilog/"
