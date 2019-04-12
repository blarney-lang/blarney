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
    -- Dispatch rules for execute stage
  , execRules :: State -> [Instr -> Action ()]
    -- Dispatch rules for write-back stage
  , writeBackRules :: State -> [Instr -> Action ()]
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

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Instruction memory
  instrMem :: RAM InstrAddr Instr <- makeRAMInit "prog.mif"

  -- Two block RAMs allows two operands to be read,
  -- and one result to be written, on every cycle
  regFileA :: RAM RegId (Bit 32) <- makeDualRAMForward 0
  regFileB :: RAM RegId (Bit 32) <- makeDualRAMForward 0

  -- Wire used to overidge the update to the PC,
  -- in case of a branch instruction
  pcNext :: Wire (Bit 32) <- makeWire dontCare

  -- Stage 0 (Instruction Fetch) registers / wires
  pc_0_1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  -- Always trigger stage 1, except on first cycle
  let go_0_1 :: Bit 1 = reg 0 1

  -- Stage 1 (Operand Fetch) registers / wires
  pc_1_2    :: Reg (Bit 32) <- makeReg dontCare
  instr_1_2 :: Reg Instr    <- makeReg dontCare
  go_1_2    :: Reg (Bit 1)  <- makeDReg 0

  -- Stage 2 (Latch Operands) registers / wires
  pc_2_3    :: Reg (Bit 32) <- makeReg dontCare
  instr_2_3 :: Reg Instr    <- makeReg dontCare
  go_2_3    :: Reg (Bit 1)  <- makeDReg 0
  regA_2_3  :: Reg (Bit 32) <- makeReg dontCare
  regB_2_3  :: Reg (Bit 32) <- makeReg dontCare

  -- Stage 3 (Execute) registers / wires
  pc_3_4     :: Reg (Bit 32)        <- makeReg dontCare
  instr_3_4  :: Reg Instr           <- makeReg dontCare
  go_3_4     :: Reg (Bit 1)         <- makeDReg 0
  regRes_3_4 :: Reg (Bit 1, Bit 32) <- makeReg (0, dontCare)
  regA_3_4   :: Reg (Bit 32)        <- makeReg dontCare
  wireRes_3  :: Wire (Bit 32)       <- makeWire dontCare

  -- Stage 4 (WriteBack) registers / wires
  wireInnerRes_4 :: Wire (Bit 32) <- makeWire dontCare
  wireFinalRes_4 :: Wire (Bit 32) <- makeWire dontCare

  -- helpers
  let rdNonZero instr = (c.dst) instr .!=. 0
  let isDebug = testPlusArgs "DEBUG"

  always do
    -- Stage 0: Instruction Fetch
    -- =========================================================================

    -- PC to fetch
    let pcFetch = pcNext.active ? (pcNext.val, pc_0_1.val + 4)

    -- Index the instruction memory
    let instrAddr = lower (range @31 @2 pcFetch)
    load instrMem instrAddr

    when isDebug do
      display "--------------------------------------------"
      display "Stage 0 -- pc: 0x%08x" (pcFetch)

    -- Latch PC for next stage
    pc_0_1 <== pcFetch

    -- Stage 1: Operand Fetch
    -- =========================================================================

    when go_0_1 do
      when isDebug $ display "Stage 1 -- pc: 0x%08x" (pc_0_1.val)
                             ", inst: 0x%08x" (instrMem.out)
      -- Trigger stage 2, except on pipeline flush
      when (pcNext.active.inv) do
        go_1_2 <== 1

    -- Fetch operands
    load regFileA (c.srcA $ instrMem.out)
    load regFileB (c.srcB $ instrMem.out)

    -- Latch PC and instruction for next stage
    pc_1_2    <== pc_0_1.val
    instr_1_2 <== instrMem.out

    -- Stage 2: Latch Operands
    -- =========================================================================
  
    -- Register forwarding logic
    let forward rS other =
          if wireRes_3.active
             .&. ((c.dst $ instr_2_3.val) .==. instr_1_2.val.rS) then
          wireRes_3.val
          else if wireFinalRes_4.active
                  .&. ((c.dst $ instr_3_4.val) .==. instr_1_2.val.rS) then
          wireFinalRes_4.val
          else other

    -- Register forwarding
    let a = forward (c.srcA) (regFileA.out)
    let b = forward (c.srcB) (regFileB.out)

    when (go_1_2.val) do
      when isDebug $ display "Stage 2 -- pc: 0x%08x" (pc_1_2.val)
                             ", inst: 0x%08x" (instr_1_2.val)
                             ", latching a: 0x%08x" a " and b: 0x%08x" b
      -- Trigger stage 3, except on pipeline flush
      when (pcNext.active.inv) do
        go_2_3 <== go_1_2.val

    -- Latch PC, instruction and operands for next stage
    pc_2_3    <== pc_1_2.val
    instr_2_3 <== instr_1_2.val
    regA_2_3  <== a
    regB_2_3  <== b

    -- Stage 3: Execute
    -- =========================================================================

    -- State for execute stage
    let state = State {
            opA    = regA_2_3.val
          , opB    = regB_2_3.val
          , pc     = ReadWrite (pc_2_3.val) (pcNext <==)
          , result = WriteOnly (\x -> when (rdNonZero (instr_2_3.val))
                                        (wireRes_3 <== x))
          }
    -- on active execute stage
    when (go_2_3.val) do
      when isDebug $ display "Stage 3 -- pc: 0x%08x" (pc_2_3.val)
                             ", inst: 0x%08x" (instr_2_3.val)
                             ", regA_2_3.val: 0x%08x" (regA_2_3.val)
                             ", regB_2_3.val: 0x%08x" (regB_2_3.val)
                             ", wireRes_3: (%0d, 0x%08x)"
                             (wireRes_3.active) (wireRes_3.val)
      -- Instruction dispatch
      match (instr_2_3.val) (c.execRules $ state)

    -- always trigger next stage
    go_3_4 <== go_2_3.val

    -- Latch PC, instruction, operand A and result for next stage
    pc_3_4     <== pc_2_3.val
    instr_3_4  <== instr_2_3.val
    regA_3_4   <== regA_2_3.val
    regRes_3_4 <== (wireRes_3.active, wireRes_3.val)

    -- Stage 4: WriteBack
    -- =========================================================================
    -- State for pre-execute stage
    let state = State {
            opA    = regA_3_4.val
          , opB    = error "can't access opB in write-back"
          , pc     = error "can't access pc in write-back"
          , result = WriteOnly (\x -> when (rdNonZero (instr_3_4.val))
                                        (wireInnerRes_4 <== x))
          }
    -- on active write back stage
    when (go_3_4.val) do
      -- Instruction dispatch
      match (instr_3_4.val) (c.writeBackRules $ state)
      -- Select final result value
      if wireInnerRes_4.active then
        wireFinalRes_4 <== wireInnerRes_4.val
      else if (fst $ regRes_3_4.val) then
        wireFinalRes_4 <== (snd $ regRes_3_4.val)
      else noAction
      when isDebug $ display "Stage 4 -- pc: 0x%08x" (pc_3_4.val)
                             ", inst: 0x%08x" (instr_3_4.val)
      -- write value back to the register file
      let rd = dst c (instr_3_4.val)
      when (wireFinalRes_4.active) do
        store regFileA rd (wireFinalRes_4.val)
        store regFileB rd (wireFinalRes_4.val)
        when isDebug $ display "Stage 4 -- reg#%0d" rd
                               " <= 0x%08x" (wireFinalRes_4.val)
