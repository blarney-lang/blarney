import Blarney
import System.Environment

-- Tiny 8-bit CPU with a 4-stage pipeline
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00DDNNNN  | Write value 0000NNNN to register DD
-- 01DDAABB  | Add register AA to register BB and store in register DD
-- 10NNNNBB  | Branch back by NNNN instructions if BB is non-zero
-- 11NNNNNN  | Halt

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
  regFileA :: RAM RegId (Bit 8) <- makeDualRAMForward
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMForward

  -- Instruction register
  instr :: Reg (Bit 8) <- makeReg dontCare

  -- Instruction operand registers
  opA :: Reg (Bit 8) <- makeReg dontCare
  opB :: Reg (Bit 8) <- makeReg dontCare

  -- Program counter
  pcNext :: Wire (Bit 8) <- makeWire 0
  let pc = reg 0 pcNext.val

  -- Result of the execute stage
  result :: Wire (Bit 8) <- makeWire 0

  -- Wire to trigger a pipeline flush
  flush :: Wire (Bit 1) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always do count <== count.val + 1

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- Index the instruction memory
    instrMem.load pcNext.val

    -- Start the pipeline after one cycle
    go1 <== 1

    -- Stage 1: Operand Fetch
    -- ======================

    when go1.val do
      when (inv flush.val) do
        pcNext <== pc + 1
        go2 <== 1

    regFileA.load (rA instrMem.out)
    regFileB.load (rB instrMem.out)

    -- Stage 2: Latch Operands
    -- =======================

    -- Latch instruction
    instr <== old instrMem.out

    -- Register forwarding logic
    let forward rS other =
          (result.active .&. (rD instr.val .==. rS (old instrMem.out))) ?
          (result.val, other)

    -- Latch operands
    opA <== forward rA regFileA.out
    opB <== forward rB regFileB.out

    -- Trigger stage 3
    when (inv flush.val) do
      go3 <== go2.val

    -- Stage 3: Execute
    -- ================

    -- Instruction dispatch
    when go3.val do
      switch (opcode instr.val)
        [
          -- Load-immediate instruction
          0b00 --> result <== zeroExtend (imm instr.val),
          -- Add instruction
          0b01 --> result <== opA.val + opB.val,
          -- Branch instruction
          0b10 --> when (opB.val .!=. 0) do
                     pcNext <== pc - zeroExtend (offset instr.val) - 2
                     -- Control hazard
                     flush <== 1,
          -- Halt instruction
          0b11 --> finish
        ]

      -- Writeback
      when (result.active) do
        regFileA.store (rD instr.val) result.val
        regFileB.store (rD instr.val) result.val
        display (formatDec 8 count.val)
                ": rf[" (rD instr.val) "] := " result.val

-- Main function
main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate makeCPU
     | otherwise -> writeVerilogTop makeCPU "CPU" "CPU-Verilog/"
