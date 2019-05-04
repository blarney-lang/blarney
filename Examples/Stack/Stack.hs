-- This module implements a full-throughput stack that always allows
-- read access to the top two items.

import Blarney
import Blarney.RAM

-- Interface
-- =========

-- Stack of 2^n items of type a
data Stack n a =
  Stack {
    -- Push given item
    push  :: a -> Action ()
    -- Push nth item from top
  , copy  :: Bit n -> Action ()
    -- Pop any number of items
  , pop   :: Bit n -> Action ()
    -- Top two stack values
  , top1  :: a
  , top2  :: a
  }

-- Features:
-- Parallel push and pop: yes
-- Parallel push and copy: no
-- Parallel pop and copy: no

-- Implementation
-- ==============

makeStack :: (Bits a, KnownNat n) => Module (Stack n a)
makeStack = do
  -- True dual port RAM
  (ram1, ram2) <- makeTrueDualRAM

  -- Top two elements stored in registers
  reg1 :: Reg a <- makeReg dontCare
  reg2 :: Reg a <- makeReg dontCare

  -- Pointer to top of stack
  sp :: Reg (Bit n) <- makeReg dontCare

  -- When these signals are high, the RAM holds the
  -- top stack elements, not the registers
  unlatched1 :: Reg (Bit 1) <- makeDReg 0
  unlatched2 :: Reg (Bit 1) <- makeDReg 0
  let topVal1 = unlatched1.val ? (ram1.out, reg1.val)
  let topVal2 = unlatched2.val ? (ram2.out, reg2.val)

  -- Interface wires
  pushWire <- makeWire dontCare
  copyWire <- makeWire dontCare
  popWire <- makeWire 0
  let pushOrCopy = pushWire.active .|. copyWire.active

  always do
    -- Update stack pointer
    sp <== (sp.val - popWire.val) + (pushWire.active ? (1, 0))

    -- Pushing and not popping
    when (pushOrCopy .&. popWire.active.inv) do
      if (pushWire.active)
        then reg1 <== pushWire.val
        else do
          if copyWire.val .==. 0
            then reg1 <== topVal1
            else if copyWire.val .==. 1
                   then reg1 <== topVal2
                   else unlatched1 <== true
      reg2 <== topVal1
      store ram2 (sp.val) topVal2
      load ram1 (sp.val - copyWire.val + 1)

    -- Popping and not pushing
    when (popWire.active .&. pushWire.active.inv) do
      unlatched2 <== true
      load ram2 (sp.val - popWire.val)
      if popWire.val .==. 1
        then reg1 <== topVal2
        else do
          unlatched1 <== true
          load ram1 (sp.val - popWire.val + 1)

    -- Pushing and popping
    when (pushWire.active .&. popWire.active) do
      reg1 <== pushWire.val
      reg2 <== topVal2
      when (popWire.val .!=. 1) do
        unlatched2 <== true
        load ram2 (sp.val - popWire.val + 1)

    -- Neither pushing, nor popping
    when (pushOrCopy.inv .&. popWire.active.inv) do
      reg1 <== topVal1
      reg2 <== topVal2

  return $
    Stack {
      push = \a -> pushWire <== a
    , copy = \n -> copyWire <== n
    , pop  = \n -> popWire <== n
    , top1 = topVal1
    , top2 = topVal2
    }

-- Test bench
-- ==========

top :: Module ()
top = do
  -- Create 256-element stack
  stk :: Stack 8 (Bit 8) <- makeStack

  -- Sample test sequence
  let test =
        Seq [
          Action do push stk 1
        , Action do push stk 2
        , Action do push stk 3
        , Action do
            push stk 4
            display (stk.top1) " " (stk.top2)
        , Action do
            copy stk 3
            display (stk.top1) " " (stk.top2)
        , Action do
            display (stk.top1) " " (stk.top2)
        , Action do
            display (stk.top1) " " (stk.top2)
        ]

  runOnce test

-- Code generation
-- ===============

main :: IO ()
main = writeVerilogTop top "top" "Stack-Verilog/"
