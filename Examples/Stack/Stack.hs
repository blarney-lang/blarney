-- This module implements a full-throughput dual-port stack.

import Blarney
import Blarney.Recipe

-- Interface
-- =========

-- Stack of 2^n items of type a
data Stack n a =
  Stack {
    -- Can push one or two items per cycle:
    --    * push2 not allowed without push1
    --    * push1 happens after push2
    push1 :: a -> Action ()
  , push2 :: a -> Action ()
    -- Push nth item from top
  , copy  :: Bit n -> Action ()
    -- Pop any number of items
  , pop   :: Bit n -> Action ()
    -- Top two stack values
  , top1  :: a
  , top2  :: a
  }

-- Methods:
-- Parallel push1 and pop: yes
-- Parallel push2 and pop: no
-- Parallel push1/push2 and copy: no
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
  sp :: Reg (Bit n) <- makeReg 0

  -- Pointer plus one
  sp1 :: Reg (Bit n) <- makeReg 1

  -- When these signals are high, the RAM holds the
  -- top stack elements, not the registers
  unlatched1 :: Reg (Bit 1) <- makeDReg 0
  unlatched2 :: Reg (Bit 1) <- makeDReg 0
  let topVal1 = unlatched1.val ? (ram1.out, reg1.val)
  let topVal2 = unlatched2.val ? (ram2.out, reg2.val)

  -- Interface wires
  push1Wire <- makeWire dontCare
  push2Wire <- makeWire dontCare
  copyWire <- makeWire dontCare
  popWire <- makeWire 0
  let pushOrCopy = push1Wire.active .|. copyWire.active

  always do
    -- Update stack pointer
    let inc = push2Wire.active ? (2, pushOrCopy ? (1, 0))
    sp <== (sp.val - popWire.val) + inc
    sp1 <== (sp1.val - popWire.val) + inc

    -- Pushing and not popping
    when (pushOrCopy .&. popWire.active.inv) do
      if push1Wire.active
        then reg1 <== push1Wire.val
        else do
          if copyWire.val .==. 0
            then reg1 <== topVal1
            else if copyWire.val .==. 1
                   then reg1 <== topVal2
                   else unlatched1 <== true
      if push2Wire.active
        then do
          reg2 <== push2Wire.val
          store ram1 (sp1.val) topVal1
        else do
          reg2 <== topVal1
          load ram1 (sp1.val - copyWire.val)
      store ram2 (sp.val) topVal2

    -- Popping and not pushing
    when (popWire.active .&. push1Wire.active.inv) do
      unlatched2 <== true
      load ram2 (sp.val - popWire.val)
      if popWire.val .==. 1
        then reg1 <== topVal2
        else do
          unlatched1 <== true
          load ram1 (sp1.val - popWire.val)

    -- Pushing and popping
    when (push1Wire.active .&. popWire.active) do
      reg1 <== push1Wire.val
      reg2 <== topVal2
      when (popWire.val .!=. 1) do
        unlatched2 <== true
        load ram2 (sp1.val - popWire.val)

    -- Neither pushing, nor popping
    when (pushOrCopy.inv .&. popWire.active.inv) do
      reg1 <== topVal1
      reg2 <== topVal2

  return $
    Stack {
      push1 = \a -> push1Wire <== a
    , push2 = \a -> push2Wire <== a
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
          Action do
            push2 stk 1
            push1 stk 2
        , Action do push1 stk 3
        , Action do
            push1 stk 4
            display (stk.top1) " " (stk.top2)
        , Action do
            copy stk 3
            display (stk.top1) " " (stk.top2)
        , Action do
            pop stk 1
            display (stk.top1) " " (stk.top2)
        , Action do
            display (stk.top1) " " (stk.top2)
        , Action do
            display (stk.top1) " " (stk.top2)
            push1 stk 10
            pop stk 1
        , Action do
            push1 stk 11
            display (stk.top1) " " (stk.top2)
        , Action do
            pop stk 3
            display (stk.top1) " " (stk.top2)
        , Action do
            display (stk.top1) " " (stk.top2)
            finish
        ]

  runOnce test

-- Code generation
-- ===============

main :: IO ()
main = writeVerilogTop top "top" "Stack-Verilog/"
