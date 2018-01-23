module FIFO where

import Blarney

-- FIFO interface
data FIFO a =
  FIFO {
    notFull  :: Bit 1
  , notEmpty :: Bit 1
  , enq      :: a -> RTL ()
  , deq      :: RTL ()
  , first    :: a
  }

-- FIFO module (simple one-element FIFO)
makeFIFO :: forall a. Bits a => RTL (FIFO a)
makeFIFO = do
  -- Register holding the one element
  reg :: Reg a <- makeReg (unpack 0)

  -- Register defining whether or not FIFO is full
  full :: Reg (Bit 1) <- makeReg 0

  -- Wires for communicating with methods
  doDeq :: Wire (Bit 1) <- makeWire 0
  doEnq :: Wire (Bit 1) <- makeWire 0
  enqVal :: Wire a <- makeWire (unpack 0)

  -- Update register on enqueue
  when (val doEnq) $ do
    reg <== val enqVal
    full <== 1

  -- Update full flag on dequeue
  when (val doDeq) $ do
    full <== 0

  -- Methods
  let notFull = val full .==. 0

  let notEmpty = val full .==. 1

  let enq a = do
        doEnq <== 1
        enqVal <== a

  let deq = doDeq <== 1

  let first = val reg

  -- Return interface
  return (FIFO notFull notEmpty enq deq first)

-- Top-level module
makeTop :: RTL ()
makeTop = do
  -- Counter
  counter :: Reg (Bit 8) <- makeReg 0
  counter <== val counter + 1

  -- Instantiate a FIFO
  fifo :: FIFO (Bit 8) <- makeFIFO

  -- Writer side
  when (notFull fifo) $ do
    enq fifo (val counter)
    display "Enqueued " (val counter)

  -- Reader side
  when (notEmpty fifo) $ do
    deq fifo
    display "Dequeued " (first fifo)
