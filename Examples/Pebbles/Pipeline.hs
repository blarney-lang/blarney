module Pipeline where

-- 5-stage pipeline for 32-bit 3-operand register-based CPU.

import Blarney
import Blarney.BitScan

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Instruction memory size
type InstrAddr = Bit 14

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
  , preExecRules :: State -> [Instr -> Action (Bit 1)]
    -- Dispatch rules for execute stage
  , execRules :: State -> [Instr -> Action (Bit 1)]
    -- Dispatch rules for post-execute stage
  , postExecRules :: State -> [Instr -> Action (Bit 1)]
  }

-- Pipeline state, visisble to the ISA
data State =
  State {
    -- Source operands
    opA :: Bit 32
  , opB :: Bit 32
    -- Program counter interface
  , pc :: ReadWrite (Bit 32)
    -- Write the instruction result
  , result :: WriteOnly (Bit 32)
    -- Indicate late result (i.e. computed in writeback rather than execute)
  , late :: WriteOnly (Bit 1)
  }

-- Pipeline
makeCPUPipeline :: Bool -> Config -> Module ()
makeCPUPipeline sim c = do
  -- Instruction memory
  let ext = if sim then ".hex" else ".mif"
  instrMem :: RAM InstrAddr Instr <- makeRAMInit ("prog" ++ ext)

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
  postResultWire :: Wire (Bit 32) <- makeWire dontCare
  finalResultWire :: Wire (Bit 32) <- makeWire dontCare

  -- Pipeline stall
  lateWire :: Wire (Bit 1) <- makeWire 0
  stallWire :: Wire (Bit 1) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Program counters for each pipeline stage
  pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  pc2 :: Reg (Bit 32) <- makeReg dontCare
  pc3 :: Reg (Bit 32) <- makeReg dontCare

  -- Instruction registers for pipeline stages 2 and 3 and 4
  instr2 :: Reg Instr <- makeReg 0
  instr3 :: Reg Instr <- makeReg 0
  instr4 :: Reg Instr <- makeReg 0

  -- Triggers for pipeline stages 2 and 3
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0
  go4 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- PC to fetch
    let pcFetch = pcNext.active ?
                    (pcNext.val, stallWire.val ? (pc1.val, pc1.val + 4))
    pc1 <== pcFetch

    -- Index the instruction memory
    let instrAddr = lower (slice @31 @2 pcFetch)
    load instrMem instrAddr

    -- Always trigger stage 1, except on first cycle
    let go1 :: Bit 1 = reg 0 1

    -- Stage 1: Operand Fetch
    -- ======================

    -- Trigger stage 2, except on pipeline flush or stall
    when go1 do
      when (pcNext.active.inv .&. stallWire.val.inv) do
        go2 <== 1

    -- Fetch operands
    load regFileA (srcA c (instrMem.out))
    load regFileB (srcB c (instrMem.out))

    -- Latch instruction and PC for next stage
    instr2 <== instrMem.out
    pc2 <== pc1.val

    -- Stage 2: Latch Operands
    -- =======================
  
    -- Register forwarding logic
    let forward rS other =
         (resultWire.active .&. (dst c (instr3.val) .==. instr2.val.rS)) ?
         (resultWire.val, other)

    let forward' rS other =
         (finalResultWire.active .&.
           (dst c (instr4.val) .==. instr2.val.rS)) ?
             (finalResultWire.val, other)

    -- Register forwarding
    let a = forward (c.srcA) (forward' (c.srcA) (regFileA.out))
    let b = forward (c.srcB) (forward' (c.srcB) (regFileB.out))

    -- Latch operands
    regA <== a
    regB <== b

    -- State for pre-execute stage
    let state = State {
            opA    = a
          , opB    = b
          , pc     = ReadWrite (pc2.val) (error "Can't write PC in pre-execute")
          , result = error "Can't write result in pre-execute"
          , late   = WriteOnly (lateWire <==)
          }

    -- Pre-execute rules
    when (go2.val) do
      match (instr2.val) (preExecRules c state)

    -- Pipeline stall
    when (lateWire.val) do
      when ((srcA c (instrMem.out) .==. dst c (instr2.val)) .|.
            (srcB c (instrMem.out) .==. dst c (instr2.val))) do
        stallWire <== true

    -- Latch instruction and PC for next stage
    instr3 <== instr2.val
    pc3 <== pc2.val

    -- Trigger stage 3, except on pipeline flush
    when (pcNext.active.inv) do
      go3 <== go2.val

    -- Stage 3: Execute
    -- ================

    -- State for execute stage
    let state = State {
            opA    = regA.val
          , opB    = regB.val
          , pc     = ReadWrite (pc3.val) (pcNext <==)
          , result = WriteOnly $ \x ->
                       when (dst c (instr3.val) .!=. 0) do
                         resultWire <== x
          , late   = error "Cant write late signal in execute"
          }

    -- Execute rules
    when (go3.val) do
      match (instr3.val) (execRules c state)
      go4 <== go3.val

    instr4 <== instr3.val

    -- Stage 4: Writeback
    -- ==================

    -- State for post-execute stage
    let state = State {
            opA    = regA.val.old
          , opB    = regB.val.old
          , pc     = error "Can't access PC in post-execute"
          , result = WriteOnly $ \x ->
                       when (dst c (instr4.val) .!=. 0) do
                         postResultWire <== x
          , late   = error "Can't write late signal in post-execute"
          }

    -- Post-execute rules
    when (go4.val) do
      match (instr4.val) (postExecRules c state)

    -- Determine final result
    let rd = dst c (instr4.val)
    when (postResultWire.active) do
      finalResultWire <== postResultWire.val
    when (postResultWire.active.inv .&. delay 0 (resultWire.active)) do
      finalResultWire <== resultWire.val.old

    -- Writeback
    when (finalResultWire.active) do
      store regFileA rd (finalResultWire.val)
      store regFileB rd (finalResultWire.val)
