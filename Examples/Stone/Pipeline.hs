module Pipeline where

-- 4-stage pipeline, parameterised by the ISA.
-- We assume the ISA is a 32-bit 3-operand register machine.

import Blarney
import Blarney.RAM
import Blarney.BitScan

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Instruction memory size
type InstrAddr = Bit 16

-- Pipeline configuration
data Config =
  Config {
    -- Get source register A
    srcA :: Instr -> RegId
    -- Get source register B
  , srcB :: Instr -> RegId
    -- Get destination register
  , dst :: Instr -> RegId
    -- Dispatch rules for pre-execute stage
  , preExecRules :: State -> [Instr -> Action ()]
    -- Dispatch rules for execute stage
  , execRules :: State -> [Instr -> Action ()]
  }

-- Pipeline state, visisble to the ISA
data State =
  State {
    -- Source operands
    opA :: Bit 32
  , opB :: Bit 32
    -- Current PC
  , getPC :: Bit 32
    -- Write a new PC
  , setPC :: Bit 32 -> Action ()
    -- Write the instruction result
  , result :: Wire (Bit 32)
  }

-- Pipeline
makeCPUPipeline :: Config -> Module ()
makeCPUPipeline c = do
  -- Instruction memory
  instrMem :: RAM InstrAddr Instr <- makeRAMInit "prog.hex"

  -- Two block RAMs allows two operands to be read,
  -- and one result to be written, on every cycle
  regFileA :: RAM RegId (Bit 32) <- makeDualRAMForward 0
  regFileB :: RAM RegId (Bit 32) <- makeDualRAMForward 0

  -- Instruction register
  instr :: Reg Instr <- makeReg dontCare

  -- Instruction operand registers
  regA :: Reg (Bit 32) <- makeReg dontCare
  regB :: Reg (Bit 32) <- makeReg dontCare

  -- Wire used to overidge the update to the PC,
  -- in case of a branch instruction
  pcNext :: Wire (Bit 32) <- makeWire dontCare

  -- Result of the execute stage
  resultWire :: Wire (Bit 32) <- makeWire dontCare

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Program counters for each pipeline stage
  pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  pc2 :: Reg (Bit 32) <- makeReg dontCare
  pc3 :: Reg (Bit 32) <- makeReg dontCare

  -- Instruction registers for pipeline stages 2 and 3
  instr2 :: Reg Instr <- makeReg 0
  instr3 :: Reg Instr <- makeReg 0

  -- Triggers for pipeline stages 2 and 3
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- PC to fetch
    let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)
    pc1 <== pcFetch

    -- Index the instruction memory
    let instrAddr = lower (range @31 @2 pcFetch)
    load instrMem instrAddr

    -- Always trigger stage 1, except on first cycle
    let go1 :: Bit 1 = reg 0 1

    -- Stage 1: Operand Fetch
    -- ======================

    -- Trigger stage 2, except on pipeline flush
    when go1 do
      when (pcNext.active.inv) do
        go2 <== 1

    -- Fetch operands
    load regFileA (c.srcA $ instrMem.out)
    load regFileB (c.srcB $ instrMem.out)

    -- Latch instruction and PC for next stage
    instr2 <== instrMem.out
    pc2 <== pc1.val

    -- Stage 2: Latch Operands
    -- =======================
  
    -- Register forwarding logic
    let forward rS other =
         (resultWire.active .&. ((c.dst $ instr3.val) .==. instr2.val.rS)) ?
         (resultWire.val, other)

    -- Latch operands
    regA <== forward (c.srcA) (regFileA.out)
    regB <== forward (c.srcB) (regFileB.out)

    -- State for pre-execute stage
    let state = State {
            opA    = regFileA.out
          , opB    = regFileB.out
          , getPC  = pc2.val
          , setPC  = error "setPC not allowed in pre-execute"
          , result = error "result wire can't be used in pre-execute"
          }

    -- Trigger stage 3, except on pipeline flush
    when (pcNext.active.inv) do
      match (instr2.val) (c.preExecRules $ state)
      go3 <== go2.val

    -- Latch instruction and PC for next stage
    instr3 <== instr2.val
    pc3 <== pc2.val

    -- Stage 3: Execute
    -- ================

    -- State for execute stage
    let state = State {
            opA    = regA.val
          , opB    = regB.val
          , getPC  = pc3.val
          , setPC  = \pcNew -> pcNext <== pcNew
          , result = resultWire
          }

    -- Instruction dispatch
    when (go3.val) do
      match (instr3.val) (c.execRules $ state)

    if (go3.val) .==. 1
      then do
        display "C " (count.val) " pc=" (pc3.val) " instr=" (instr3.val)
        return ()
      else do
        display "_ " (count.val) " pc=" (pc3.val) " instr=" (instr3.val)
        return ()

    -- Writeback
    when (resultWire.active) do
      let rd = dst c (instr3.val)
      store regFileA rd (resultWire.val)
      store regFileB rd (resultWire.val)
      display (count.val) ": rf[" rd "] := " (resultWire.val)
