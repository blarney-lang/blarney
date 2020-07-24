module Pipeline where

-- 5-stage pipeline for 32-bit 3-operand register-based CPU.

import Blarney
import Blarney.Option
import Blarney.BitScan
import qualified Data.Map as Map

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Instruction memory size
type InstrAddr = Bit 14

-- Pipeline configuration
data Config =
  Config {
    -- Decode table
    decodeTable :: [(String, String)]
    -- Action for pre-execute stage
  , preExecRules :: State -> Action ()
    -- Action for execute stage
  , execRules :: State -> Action ()
    -- Action for post-execute stage
  , postExecRules :: State -> Action ()
  }

-- Pipeline state, visisble to the ISA
data State =
  State {
    -- Current instruction
    instr :: Bit 32
    -- Source operands
  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32
    -- Program counter interface
  , pc :: ReadWrite (Bit 32)
    -- Write the instruction result
  , result :: WriteOnly (Bit 32)
    -- Indicate late result (i.e. computed in writeback rather than execute)
  , late :: WriteOnly (Bit 1)
    -- Result of instruction decode
  , opcode :: TagMap String
  , fields :: FieldMap
  }

-- Helper function for determining opcode
infix 8 `is`
is :: (Ord tag, Show tag) => TagMap tag -> [tag] -> Bit 1
is m [] = false
is m (key:keys) =
  case Map.lookup key m of
    Nothing -> error ("Unknown opcode " ++ show key)
    Just b -> b .|. is m keys

-- Pipeline
makeCPUPipeline :: Bool -> Config -> Module ()
makeCPUPipeline sim c = do
  -- Compute field selector functions from decode table
  let selMap = matchSel (c.decodeTable)

  -- Functions for extracting register ids from an instruction
  let srcA :: Instr -> RegId = getFieldSel selMap "rs1"
  let srcB :: Instr -> RegId = getFieldSel selMap "rs2"
  let dst  :: Instr -> RegId = getFieldSel selMap "rd"

  -- Instruction memory
  let ext = if sim then ".hex" else ".mif"
  instrMem :: RAM InstrAddr Instr <- makeRAMInit ("prog" ++ ext)

  -- Two block RAMs allows two operands to be read,
  -- and one result to be written, on every cycle
  regFileA :: RAM RegId (Bit 32) <- makeDualRAMForward 0
  regFileB :: RAM RegId (Bit 32) <- makeDualRAMForward 0

  -- Instruction operand registers
  regA :: Reg (Bit 32) <- makeReg dontCare
  regB :: Reg (Bit 32) <- makeReg dontCare
  regBorImm :: Reg (Bit 32) <- makeReg dontCare

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
    load regFileA (instrMem.out.srcA)
    load regFileB (instrMem.out.srcB)

    -- Latch instruction and PC for next stage
    instr2 <== instrMem.out
    pc2 <== pc1.val

    -- Stage 2: Latch Operands
    -- =======================

    -- Decode instruction
    let (tagMap, fieldMap) = matchMap False (c.decodeTable) (instr2.val)

    -- Register forwarding logic
    let forward rS other =
         (resultWire.active .&. (instr3.val.dst .==. instr2.val.rS)) ?
         (resultWire.val, other)

    let forward' rS other =
         (finalResultWire.active .&.
           (instr4.val.dst .==. instr2.val.rS)) ?
             (finalResultWire.val, other)

    -- Register forwarding
    let a = forward srcA (forward' srcA (regFileA.out))
    let b = forward srcB (forward' srcB (regFileB.out))

    -- Use "imm" field if valid, otherwise use register b
    let bOrImm = if Map.member "imm" fieldMap
                   then let imm = getField fieldMap "imm"
                        in imm.valid ? (imm.val, b)
                   else b

    -- Latch operands
    regA <== a
    regB <== b
    regBorImm <== bOrImm

    -- State for pre-execute stage
    let state = State {
            instr = instr2.val
          , opA = a
          , opB = b
          , opBorImm = bOrImm
          , pc = ReadWrite (pc2.val) (error "Can't write PC in pre-execute")
          , result = error "Can't write result in pre-execute"
          , late = WriteOnly (lateWire <==)
          , opcode = tagMap
          , fields = fieldMap
          }

    -- Pre-execute action
    when (go2.val) do
      preExecRules c state

    -- Pipeline stall
    when (lateWire.val) do
      when ((instrMem.out.srcA .==. instr2.val.dst) .|.
            (instrMem.out.srcB .==. instr2.val.dst)) do
        stallWire <== true

    -- Latch instruction and PC for next stage
    instr3 <== instr2.val
    pc3 <== pc2.val

    -- Trigger stage 3, except on pipeline flush
    when (pcNext.active.inv) do
      go3 <== go2.val

    -- Stage 3: Execute
    -- ================

    -- Buffer the decode tables
    let bufferField opt = Option (buffer (opt.valid)) (map buffer (opt.val))
    let tagMap3 = Map.map buffer tagMap
    let fieldMap3 = Map.map bufferField fieldMap

    -- State for execute stage
    let state = State {
            instr = instr3.val
          , opA = regA.val
          , opB = regB.val
          , opBorImm = regBorImm.val
          , pc = ReadWrite (pc3.val) (pcNext <==)
          , result = WriteOnly $ \x ->
                       when (instr3.val.dst .!=. 0) do
                         resultWire <== x
          , late = error "Cant write late signal in execute"
          , opcode = tagMap3
          , fields = fieldMap3
          }

    -- Execute action
    when (go3.val) do
      execRules c state
      go4 <== go3.val

    instr4 <== instr3.val

    -- Stage 4: Writeback
    -- ==================

    -- Buffer the decode tables
    let tagMap4 = Map.map buffer tagMap3
    let fieldMap4 = Map.map bufferField fieldMap3

    -- State for post-execute stage
    let state = State {
            instr = instr4.val
          , opA = regA.val.old
          , opB = regB.val.old
          , opBorImm = regBorImm.val.old
          , pc = error "Can't access PC in post-execute"
          , result = WriteOnly $ \x ->
                       when (instr4.val.dst .!=. 0) do
                         postResultWire <== x
          , late = error "Can't write late signal in post-execute"
          , opcode = tagMap4
          , fields = fieldMap4
          }

    -- Post-execute rules
    when (go4.val) do
      postExecRules c state

    -- Determine final result
    let rd = instr4.val.dst
    when (postResultWire.active) do
      finalResultWire <== postResultWire.val
    when (postResultWire.active.inv .&. delay 0 (resultWire.active)) do
      finalResultWire <== resultWire.val.old

    -- Writeback
    when (finalResultWire.active) do
      store regFileA rd (finalResultWire.val)
      store regFileB rd (finalResultWire.val)
