module Pipeline where

-- 4-stage pipeline for 32-bit 3-operand register-based CPU.

import Blarney
import Blarney.RAM
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
    -- Program counter interface
  , pc :: ReadWrite (Bit 32)
    -- Write the instruction result
  , result :: WriteOnly (Bit 32)
  }

-- Pipeline
makeCPUPipeline :: Config -> Module ()
makeCPUPipeline c = do
  -- Instruction memory
  instrMem :: RAM InstrAddr Instr <- makeRAMInit "prog.mif"

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

  -- Result wire of the execute stage
  resultWire :: Wire (Bit 32) <- makeWire dontCare
  -- Result latch of the execute stage
  resultReg :: Reg (Bit 1, Bit 32) <- makeReg (0, dontCare)
  let (activeRes, valRes) = resultReg.val

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Program counters for each pipeline stage
  pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  pc2 :: Reg (Bit 32) <- makeReg dontCare
  pc3 :: Reg (Bit 32) <- makeReg dontCare
  pc4 :: Reg (Bit 32) <- makeReg dontCare

  -- Instruction registers for pipeline stages 2, 3 and 4
  instr2 :: Reg Instr <- makeReg 0
  instr3 :: Reg Instr <- makeReg 0
  instr4 :: Reg Instr <- makeReg 0

  -- Triggers for pipeline stages 2, 3 and 4
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0
  go4 :: Reg (Bit 1) <- makeDReg 0

  -- helpers
  let rdNonZero instr = (c.dst) instr .!=. 0
  let isDebug = testPlusArgs "DEBUG"

  always do
    -- Stage 0: Instruction Fetch
    -- =========================================================================

    -- PC to fetch
    let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)
    pc1 <== pcFetch

    -- Index the instruction memory
    let instrAddr = lower (range @31 @2 pcFetch)
    load instrMem instrAddr

    -- Always trigger stage 1, except on first cycle
    let go1 :: Bit 1 = reg 0 1

    when isDebug do
      display "--------------------------------------------"
      display "Stage 0 -- pc: 0x%08x" (pcFetch)

    -- Stage 1: Operand Fetch
    -- =========================================================================

    -- Trigger stage 2, except on pipeline flush
    when go1 do
      when isDebug $ display "Stage 1 -- pc: 0x%08x" (pc1.val)
                             ", inst: 0x%08x" (instrMem.out)
      when (pcNext.active.inv) do
        go2 <== 1

    -- Fetch operands
    load regFileA (c.srcA $ instrMem.out)
    load regFileB (c.srcB $ instrMem.out)

    -- Latch instruction and PC for next stage
    instr2 <== instrMem.out
    pc2 <== pc1.val

    -- Stage 2: Latch Operands
    -- =========================================================================
  
    -- Register forwarding logic
    let forward rS other =
          if resultWire.active
             .&. ((c.dst $ instr3.val) .==. instr2.val.rS) then
          resultWire.val
          else if activeRes
                  .&. ((c.dst $ instr4.val) .==. instr2.val.rS) then
          valRes
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
          , pc     = ReadWrite (pc2.val) (error "can't write pc in pre-execute")
          , result = error "result wire can't be used in pre-execute"
          }

    -- Trigger stage 3, except on pipeline flush
    when (go2.val) do
      -- Instruction dispatch
      match (instr2.val) (c.preExecRules $ state)
      when isDebug $ display "Stage 2 -- pc: 0x%08x" (pc2.val)
                             ", inst: 0x%08x" (instr2.val)
                             ", latching a: 0x%08x" a " and b: 0x%08x" b
      when (pcNext.active.inv) do
        go3 <== go2.val

    -- Latch instruction and PC for next stage
    instr3 <== instr2.val
    pc3 <== pc2.val

    -- Stage 3: Execute
    -- =========================================================================

    -- State for execute stage
    let state = State {
            opA    = regA.val
          , opB    = regB.val
          , pc     = ReadWrite (pc3.val) (pcNext <==)
          , result = WriteOnly (\x -> when (rdNonZero (instr3.val))
                                        (resultWire <== x))
          }
    -- on active execute stage
    when (go3.val) do
      when isDebug $ display "Stage 3 -- pc: 0x%08x" (pc3.val)
                             ", inst: 0x%08x" (instr3.val)
                             ", regA.val: 0x%08x" (regA.val)
                             ", regB.val: 0x%08x" (regB.val)
                             ", resultWire: (%0d, 0x%08x)"
                             (resultWire.active) (resultWire.val)
      -- Instruction dispatch
      match (instr3.val) (c.execRules $ state)

    -- Latch instruction, PC and result, and trigger next stage
    instr4 <== instr3.val
    resultReg <== (resultWire.active, resultWire.val)
    pc4 <== pc3.val
    go4 <== go3.val

    -- Stage 4: WriteBack
    -- =========================================================================
    -- on active write back stage
    when (go4.val) do
      when isDebug $ display "Stage 4 -- pc: 0x%08x" (pc4.val)
                             ", inst: 0x%08x" (instr4.val)
      -- write value back to the register file
      let rd = dst c (instr4.val)
      when (activeRes) do
        store regFileA rd (valRes)
        store regFileB rd (valRes)
        when isDebug $ display "Stage 4 -- reg#%0d" rd " <= 0x%08x" valRes
