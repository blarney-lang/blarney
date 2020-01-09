-- Tiny 8-bit CPU with a 4-stage pipeline
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00DDNNNN  | Write value 0000NNNN to register DD
-- 01DDAABB  | Add register AA to register BB and store in register DD
-- 10NNNNBB  | Branch back by NNNN instructions if BB is non-zero
-- 11NNNNNN  | Halt

import Blarney
import System.Process

-- Instructions
type Instr = Bit 8

-- Register identifiers
type RegId = Bit 2

-- Extract opcode
opcode :: Instr -> Bit 2
opcode instr = slice @7 @6 instr

-- Extract register A
rA :: Instr -> RegId
rA instr = slice @3 @2 instr

-- Extract register B
rB :: Instr -> RegId
rB instr = slice @1 @0 instr

-- Extract destination register
rD :: Instr -> RegId
rD instr = slice @5 @4 instr

-- Extract immediate
imm :: Instr -> Bit 4
imm instr = slice @3 @0 instr

-- Extract branch offset
offset :: Instr -> Bit 4
offset instr = slice @5 @2 instr

-- CPU
makeCPU :: Module ()
makeCPU = do
  -- Instruction memory
  instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  -- Two block RAMs allows two operands to be read,
  -- and one result to be written, on every cycle
  regFileA :: RAM RegId (Bit 8) <- makeDualRAMForward 0
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMForward 0

  -- Instruction register
  instr :: Reg (Bit 8) <- makeReg dontCare

  -- Instruction operand registers
  opA :: Reg (Bit 8) <- makeReg dontCare
  opB :: Reg (Bit 8) <- makeReg dontCare

  -- Program counter
  pcNext :: Wire (Bit 8) <- makeWire 0
  let pc = reg 0 (pcNext.val)

  -- Result of the execute stage
  result :: Wire (Bit 8) <- makeWire 0

  -- Wire to trigger a pipeline flush
  flush :: Wire (Bit 1) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- Index the instruction memory
    load instrMem (pcNext.val)

    -- Start the pipeline after one cycle
    go1 <== 1

    -- Stage 1: Operand Fetch
    -- ======================

    when (go1.val) do
      when (flush.val.inv) do
        pcNext <== pc + 1
        go2 <== 1

    load regFileA (instrMem.out.rA)
    load regFileB (instrMem.out.rB)

    -- Stage 2: Latch Operands
    -- =======================
  
    -- Latch instruction
    instr <== instrMem.out.old

    -- Register forwarding logic
    let forward rS other =
          (result.active .&. (instr.val.rD .==. instrMem.out.old.rS)) ?
          (result.val, other)

    -- Latch operands
    opA <== forward rA (regFileA.out)
    opB <== forward rB (regFileB.out)

    -- Trigger stage 3
    when (flush.val.inv) do
      go3 <== go2.val

    -- Stage 3: Execute
    -- ================

    -- Instruction dispatch
    when (go3.val) do
      switch (instr.val.opcode)
        [
          -- Load-immediate instruction
          0b00 --> result <== zeroExtend (instr.val.imm),
          -- Add instruction
          0b01 --> result <== opA.val + opB.val,
          -- Branch instruction
          0b10 --> when (opB.val .!=. 0) do
                     pcNext <== pc - zeroExtend (instr.val.offset) - 2
                     -- Control hazard
                     flush <== 1,
          -- Halt instruction
          0b11 --> finish
        ]

      -- Writeback
      when (result.active) do
        store regFileA (instr.val.rD) (result.val)
        store regFileB (instr.val.rD) (result.val)
        display "%08d" (count.val) ": rf[%0d]" (instr.val.rD) " := 0x%02x" (result.val)

-- Main function
main :: IO ()
main = do
  writeVerilogTop makeCPU "top" "CPU-Verilog/"
  system "cp instrs.hex CPU-Verilog/"
  return ()
