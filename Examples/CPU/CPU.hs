-- Tiny 8-bit CPU with a 3-stage pipeline
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00ZZNNNN  | Write value 0000NNNN to register ZZ
-- 01ZZXXYY  | Add register XX to register YY and store in register ZZ
-- 10NNNNYY  | Branch back by NNNN instructions if YY is non-zero
-- 11NNNNNN  | Halt

import Blarney
import Blarney.RAM
import System.Process

-- Instructions
type Instr = Bit 8

-- Register identifiers
type RegId = Bit 2

-- Extract opcode
opcode :: Instr -> Bit 2
opcode instr = range @7 @6 instr

-- Extract register A
rA :: Instr -> RegId
rA instr = range @3 @2 instr

-- Extract register B
rB :: Instr -> RegId
rB instr = range @1 @0 instr

-- Extract destination register
rD :: Instr -> RegId
rD instr = range @5 @4 instr

-- Extract immediate
imm :: Instr -> Bit 4
imm instr = range @3 @0 instr

-- Extract branch offset
offset :: Instr -> Bit 4
offset instr = range @5 @2 instr

-- CPU
makeCPU :: RTL ()
makeCPU = do
  -- Instruction memory
  instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  -- Register file
  regFileA :: RAM RegId (Bit 8) <- makeDualRAMPassthrough
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMPassthrough

  -- Instruction register
  instr :: Reg (Bit 8) <- makeRegU

  -- Instruction operand registers
  opA :: Reg (Bit 8) <- makeRegU
  opB :: Reg (Bit 8) <- makeRegU

  -- Program counter
  pcNext :: Wire (Bit 8) <- makeWire 0
  let pc = reg 0 (pcNext.val)

  -- Index the instruction memory
  load instrMem (pcNext.val)

  -- Result of the execute stage
  result :: Wire (Bit 8) <- makeWire 0

  -- Wire to trigger a pipeline flush
  flush :: Wire (Bit 1) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeRegU
  count <== count.val + 1

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  -- Start the pipeline after one cycle
  go1 <== 1

  -- Stage 1: Instruction/Operand Fetch
  -- ==================================

  when (go1.val) $ do
    when (flush.val.inv) $ do
      pcNext <== pc + 1
      go2 <== 1

  load regFileA (instrMem.out.rA)
  load regFileB (instrMem.out.rB)

  -- Stage 2: Decode
  -- ===============
  
  -- Latch instruction
  instr <== instrMem.out'

  -- Register forwarding logic
  let forward rS other =
        (result.active .&. (instr.val.rD .==. instrMem.out'.rS)) ?
        (result.val, other)

  -- Latch operands
  opA <== forward rA (regFileA.out)
  opB <== forward rB (regFileB.out)

  -- Trigger stage 3
  when (flush.val.inv) $ do
    go3 <== go2.val

  -- Stage 3: Execute
  -- ================

  -- Instruction dispatch
  when (go3.val) $ do
    switch (instr.val.opcode)
      [
        -- Load-immediate instruction
        0 --> result <== zeroExtend (instr.val.imm),
        -- Add instruction
        1 --> result <== opA.val + opB.val,
        -- Branch instruction
        2 --> do when (opB.val .!=. 0) $ do
                   pcNext <== pc - zeroExtend (instr.val.offset) - 2
                   -- Control hazard
                   flush <== 1,
        -- Halt instruction
        3 --> finish
      ]

    -- Writeback
    when (result.active) $ do
      store regFileA (instr.val.rD) (result.val)
      store regFileB (instr.val.rD) (result.val)
      display (count.val) ": rf[" (instr.val.rD) "] := " (result.val)

-- Main function
main :: IO ()
main = do
  writeVerilogTop makeCPU "top" "CPU-Verilog/"
  system "cp instrs.hex CPU-Verilog/"
  return ()
