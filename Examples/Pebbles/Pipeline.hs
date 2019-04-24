module Pipeline where

-- 5-stage pipeline for 32-bit 3-operand register-based CPU.

import Blarney
import Blarney.RAM
import Blarney.BitScan
import Blarney.Option

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Instruction memory size
type InstrAddr = Bit 14

-- Branch-Target-Buffer (BTB) mapping
type BTBIndex = Bit 8
type BTBEntry = Option InstrAddr

-- Pipeline configuration
data Config =
  Config {
    -- Get source register A
    srcA :: Instr -> RegId
    -- Get source register B
  , srcB :: Instr -> RegId
    -- Get destination register
  , dst :: Instr -> RegId
    -- Is it a branch or jump instruction?
  , isBranch :: Instr -> Bit 1
    -- Dispatch rules for pre-execute stage
  , preExecRules :: State -> [Instr -> Action ()]
    -- Dispatch rules for execute stage
  , execRules :: State -> [Instr -> Action ()]
    -- Dispatch rules for post-execute stage
  , postExecRules :: State -> [Instr -> Action ()]
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

  -- BTB for dynamic branch prediction
  btb :: RAM BTBIndex BTBEntry <- makeDualRAM

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

  -- Pipeline flush
  flushWire :: Wire (Bit 32) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Program counters for each pipeline stage
  pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  pc2 :: Reg (Bit 32) <- makeReg dontCare
  pc3 :: Reg (Bit 32) <- makeReg dontCare

  -- Instruction registers for each pipeline stage
  instr2 :: Reg Instr <- makeReg 0
  instr3 :: Reg Instr <- makeReg 0
  instr4 :: Reg Instr <- makeReg 0

  -- Triggers for each pipeline stage
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0
  go4 :: Reg (Bit 1) <- makeDReg 0

  -- Predicted branch target
  predWire :: Wire (Bit 32) <- makeWire 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- PC to fetch
    let pcFetch = flushWire.active ?
                    (flushWire.val, stallWire.val ? (pc1.val, predWire.val))
    pc1 <== pcFetch

    -- Index the instruction memory
    let getInstrAddr pc = truncate (range @31 @2 pc)
    let instrAddr = getInstrAddr pcFetch
    load instrMem instrAddr

    -- Index the BTB
    load btb (truncate instrAddr)

    -- Always trigger stage 1, except on first cycle
    let go1 :: Bit 1 = reg 0 1

    -- Stage 1: Operand Fetch
    -- ======================

    -- Trigger stage 2, except on pipeline flush or stall
    when go1 do
      when (flushWire.active.inv .&. stallWire.val.inv) do
        go2 <== 1

    -- Fetch operands
    load regFileA (srcA c (instrMem.out))
    load regFileB (srcB c (instrMem.out))

    -- Dynamic branch prediction
    if isBranch c (instrMem.out) .&. btb.out.valid
      then predWire <== zeroExtend (btb.out.val) # (0 :: Bit 2)
      else predWire <== pc1.val + 4

    -- Latch instruction and PC for next stage
    instr2 <== instrMem.out
    pc2 <== pc1.val

    -- Stage 2: Latch Operands
    -- =======================
  
    -- Register forwarding logic
    let forward getSrc other =
          let rs = instr2.val.getSrc in
            if resultWire.active .&. (dst c (instr3.val) .==. rs)
            then resultWire.val
            else if finalResultWire.active .&. (dst c (instr4.val) .==. rs)
                 then finalResultWire.val
                 else other

    -- Register forwarding
    let a = forward (c.srcA) (regFileA.out)
    let b = forward (c.srcB) (regFileB.out)

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

    -- Has instruction indicated that it's result will be computed in
    -- writeback instead of execute?
    when (lateWire.val) do
      -- Only stall if there will be a writeback->execute data hazard
      when ((srcA c (instrMem.out) .==. dst c (instr2.val)) .|.
            (srcB c (instrMem.out) .==. dst c (instr2.val))) do
        stallWire <== true

    -- Latch for next stage
    instr3 <== instr2.val
    pc3 <== pc2.val

    -- Trigger stage 3, except on pipeline flush
    when (flushWire.active.inv) do
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

    when (go3.val) do
      -- Execute rules
      match (instr3.val) (execRules c state)

      -- Check validity of branch prediction
      let correct = pcNext.active ? (pcNext.val, pc3.val + 4)
      when (pc2.val .!=. correct) do
        flushWire <== correct

      -- Update BTB
      when (isBranch c (instr3.val)) do
        store btb (getInstrAddr (pc3.val))
                  (Option (pcNext.active, getInstrAddr (pcNext.val)))

      -- Trigger writeback
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
