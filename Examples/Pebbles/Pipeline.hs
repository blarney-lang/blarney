module Pipeline
  (
  Config(..), State(..), makeCPUPipeline
  ) where

-- 4-stage pipeline for 32-bit 3-operand register-based CPU.

import Blarney
import Blarney.RAM
import Blarney.Option
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

-- inter stages communication state
data Stg1 = Stg1 {
  go :: Bit 1
, pc :: Bit 32
} deriving (Generic, Bits, FShow)

data Stg2 = Stg2 {
  go    :: Bit 1
, pc    :: Bit 32
, instr :: Instr
} deriving (Generic, Bits, FShow)

data Stg3 = Stg3 {
  go    :: Bit 1
, pc    :: Bit 32
, instr :: Instr
, regA  :: Bit 32
, regB  :: Bit 32
} deriving (Generic, Bits, FShow)

data Stg4 = Stg4 {
  go         :: Bit 1
, pc         :: Bit 32
, instr      :: Instr
, regA       :: Bit 32
, lastResult :: Option (Bit 32)
} deriving (Generic, Bits, FShow)

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

  -- Stage 0 (Instruction Fetch) to Stage 1 (Operand Fetch) register
  stg1Reg :: Reg (Option Stg1) <- makeReg (some $ Stg1 false 0xfffffffc)
  let ok1 = stg1Reg.val.valid
  let Stg1 go1 pc1 = stg1Reg.val.val
  -- Stage 1 (Operand Fetch) to Stage 2 (Latch Operands) register
  stg2Reg :: Reg (Option Stg2) <- makeReg none
  let ok2 = stg2Reg.val.valid
  let Stg2 go2 pc2 instr2 = stg2Reg.val.val
  -- Stage 2 (Latch Operands) to Stage 3 (Execute) register
  stg3Reg :: Reg (Option Stg3) <- makeReg none
  let ok3 = stg3Reg.val.valid
  let Stg3 go3 pc3 instr3 regA3 regB3 = stg3Reg.val.val
  -- Stage 3 (Execute) to Stage 4 (WriteBack) register
  stg4Reg :: Reg (Option Stg4) <- makeReg none
  let ok4 = stg4Reg.val.valid
  let Stg4 go4 pc4 instr4 regA4 lastResult4 = stg4Reg.val.val

  -- Stage 3 (Execute) wires
  wireRes_3      :: Wire (Bit 32) <- makeWire dontCare
  -- Stage 4 (WriteBack) wires
  wireInnerRes_4 :: Wire (Bit 32) <- makeWire dontCare
  wireFinalRes_4 :: Wire (Bit 32) <- makeWire dontCare

  -- helpers
  let rdNonZero instr = (c.dst) instr .!=. 0
  let isDebug = testPlusArgs "DEBUG"

  always do

    -- Common debug
    -- =========================================================================
    when isDebug do
      display "================================================================"
      display "  (ok1:" ok1 ", go1:" go1 ")"
              "  (ok2:" ok2 ", go2:" go2 ")"
              "  (ok3:" ok3 ", go3:" go3 ")"
              "  (ok4:" ok4 ", go4:" go4 ")"

    -- Stage 0: Instruction Fetch
    -- =========================================================================

    -- PC to fetch
    let pcFetch = pcNext.active ? (pcNext.val, pc1 + 4)

    -- Index the instruction memory
    let instrAddr = lower (range @31 @2 pcFetch)

    -- Stage's actions
    load instrMem instrAddr
    stg1Reg <== (some $ Stg1 true pcFetch)

    -- Stage's debug
    when isDebug do
      display "Stage 0 -- pc: 0x%08x" (pcFetch)


    -- Stage 1: Operand Fetch
    -- =========================================================================

    -- Stage's actions
    when (ok1 .&. go1) do
      -- Fetch operands
      load regFileA (c.srcA $ instrMem.out)
      load regFileB (c.srcB $ instrMem.out)
      -- Trigger stage 2, except on pipeline flush
      if (pcNext.active) then do stg2Reg <== none
      else do stg2Reg <== (some $ Stg2 {
                            go    = go1
                          , pc    = pc1
                          , instr = instrMem.out
                          })

    -- Stage's debug
    when isDebug do
      if (ok1) then do
        display "Stage 1 -- " (stg1Reg.val.val :: Stg1)
      else do
        display "Stage 1 -- -- -- -- -- -- -- --"

    -- Stage 2: Latch Operands
    -- =========================================================================
  
    -- Register forwarding logic
    let forward rS other =
          if wireRes_3.active
             .&. ((c.dst $ instr3) .==. instr2.rS) then
          wireRes_3.val
          else if wireFinalRes_4.active
                  .&. ((c.dst $ instr4) .==. instr2.rS) then
          wireFinalRes_4.val
          else other

    -- Register forwarding
    let a = forward (c.srcA) (regFileA.out)
    let b = forward (c.srcB) (regFileB.out)

    -- Stage's actions
    when (ok2 .&. go2) do
      -- Trigger stage 3, except on pipeline flush
      if (pcNext.active) then do stg3Reg <== none
      else do stg3Reg <== (some $ Stg3 {
                            go    = go2
                          , pc    = pc2
                          , instr = instr2
                          , regA  = a
                          , regB  = b
                          })

    -- Stage's debug
    when isDebug do
      if (ok2) then do
        display "Stage 2 -- " (stg2Reg.val.val :: Stg2)
      else do
        display "Stage 2 -- -- -- -- -- -- -- --"

    -- Stage 3: Execute
    -- =========================================================================

    -- State for execute stage
    let state = State {
            opA    = regA3
          , opB    = regB3
          , pc     = ReadWrite pc3 (pcNext <==)
          , result = WriteOnly (\x -> when (rdNonZero instr3)
                                        (wireRes_3 <== x))
          }

    -- Stage's actions
    when (ok3 .&. go3) do
      -- Instruction dispatch
      match instr3 (c.execRules $ state)
      -- always trigger next stage
      stg4Reg <== (some $ Stg4 {
                            go    = go3
                          , pc    = pc3
                          , instr = instr3
                          , regA  = regA3
                          , lastResult = Option (wireRes_3.active, wireRes_3.val)
                          })

    -- Stage's debug
    when isDebug do
      if (ok3) then do
        display "Stage 3 -- " (stg3Reg.val.val :: Stg3)
      else do
        display "Stage 3 -- -- -- -- -- -- -- --"

    -- Stage 4: WriteBack
    -- =========================================================================

    -- State for write-back stage
    let state = State {
            opA    = regA4
          , opB    = error "can't access opB in write-back"
          , pc     = error "can't access pc in write-back"
          , result = WriteOnly (\x -> when (rdNonZero instr4)
                                        (wireInnerRes_4 <== x))
          }
    -- destination register index
    let rd = c.dst $ instr4

    -- Stage's actions
    when (ok4 .&. go4) do
      -- Instruction dispatch
      match instr4 (c.writeBackRules $ state)
      -- Select final result value
      if wireInnerRes_4.active then
        wireFinalRes_4 <== wireInnerRes_4.val
      else if (lastResult4.valid) then
        wireFinalRes_4 <== (lastResult4.val)
      else noAction
      -- write value back to the register file
      when (wireFinalRes_4.active) do
        store regFileA rd (wireFinalRes_4.val)
        store regFileB rd (wireFinalRes_4.val)

    -- Stage's debug
    when isDebug do
      if (ok4) then do
        display "Stage 4 -- " (stg4Reg.val.val :: Stg4)
        when (wireFinalRes_4.active) do
          display "Stage 4 -- reg#%0d" rd " <= 0x%08x" (wireFinalRes_4.val)
      else do
        display "Stage 4 -- -- -- -- -- -- -- --"
