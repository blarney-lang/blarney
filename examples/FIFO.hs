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

-- FIDO module (simple one-element FIFO)
makeFIFO :: forall n. KnownNat n => RTL (FIFO (Bit n))
makeFIFO = do
  -- Register holding the one element
  reg :: Reg n <- makeReg 0

  -- Register definings whether or not FIFO is full
  full :: Reg 1 <- makeReg 0

  -- Wires for communicating with methods
  doDeq :: Wire 1 <- makeWire 0
  doEnq :: Wire 1 <- makeWire 0
  enqVal :: Wire n <- makeWire 0

  -- Update register on enqueue
  when (val doEnq) $
    reg <== val enqVal
    full <== 1

  -- Update full flag on dequeue
  when (val doDeq) $
    full <== 0

  -- Methods
  let notFull = val full .==. 0

  let notEmpty = val full .==. 1

  let enq a = do
    doEnq <== 1
    enqVal <== a

  let deq = do
    doDeq <== 1

  let first = val reg

  -- Return interface
  return (FIFO notFull notEmpty enq deq first)
