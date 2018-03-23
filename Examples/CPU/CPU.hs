-- Tiny 8-bit CPU with a 3-stage pipeline
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- ZZNNNN00  | Write value 0000NNNN to register ZZ
-- ZZXXYY01  | Add register XX to register YY and store in register ZZ
-- NNNNYY10  | Branch back by NNNN instructions if YY is non-zero

import Blarney

-- Instructions
type Instr = Bit 8

-- Register identifiers
type RegId = Bit 2

-- Extract opcode
opcode :: Instr -> Bit 2
opcode instr = instr!bits(1,0)

-- Extract register A
rA :: Instr -> RegId
rA instr = instr!bits(5,4)

-- Extract register B
rB :: Instr -> RegId
rB instr = instr!bits(3,2)

-- Extract destination register
rD :: Instr -> RegId
rD instr = instr!bits(7,6)

-- Extract immediate
imm :: Instr -> Bit 4
imm instr = instr!bits(5,2)

-- Extract branch offset
offset :: Instr -> Bit 4
offset instr = instr!bits(7,4)

-- CPU
makeCPU :: RTL ()
makeCPU = do
  -- Instruction memory
  instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  -- Register file
  regFileA :: RAM RegId (Bit 8) <- makeDualRAMPassthrough
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMPassthrough

  -- Instruction register
  instr :: Reg (Bit 8) <- makeReg

  -- Instruction operand registers
  opA :: Reg (Bit 8) <- makeReg
  opB :: Reg (Bit 8) <- makeReg

  -- Program counter
  pcNext :: Wire (Bit 8) <- makeWireDefault 0
  let pc = reg 0 (pcNext!val)

  -- Index the instruction memory
  load instrMem (pcNext!val)

  -- Result of the execute stage
  result :: Wire (Bit 8) <- makeWireDefault 0

  -- Wire to trigger a pipeline flush
  flush :: Wire (Bit 1) <- makeWireDefault 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg
  count <== count!val + 1

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  -- Start the pipeline after one cycle
  go1 <== 1

  -- Stage 1: Instruction/Operand Fetch
  -- ==================================

  when (go1!val) $ do
    when (flush!val!inv) $ do
      pcNext <== pc + 1
      go2 <== 1

  load regFileA (instrMem!out!rA)
  load regFileB (instrMem!out!rB)

  -- Stage 2: Decode
  -- ===============
  
  -- Latch instruction
  instr <== instrMem!out'

  -- Register forwarding logic
  let forward rS other =
        (result!active .&. (instr!val!rD .==. instrMem!out'!rS)) ?
        (result!val, other)

  -- Latch operands
  opA <== forward rA (regFileA!out)
  opB <== forward rB (regFileB!out)

  -- Trigger stage 3
  when (flush!val!inv) $ do
    go3 <== go2!val

  -- Stage 3: Execute
  -- ================

  -- Instruction dispatch
  when (go3!val) $ do
    switch (instr!val!opcode)
      [
        -- Load-immediate instruction
        0 --> result <== zeroExtend (instr!val!imm),
        -- Add instruction
        1 --> result <== opA!val + opB!val,
        -- Branch instruction
        2 --> do when (opB!val .!=. 0) $ do
                   pcNext <== pc - zeroExtend (instr!val!offset) - 2
                   -- Control hazard
                   flush <== 1
      ]

    -- Writeback
    when (result!active) $ do
      store regFileA (instr!val!rD) (result!val)
      store regFileB (instr!val!rD) (result!val)
      display (count!val) ": rf[" (instr!val!rD) "] := " (result!val)

-- Main function
main :: IO ()
main = netlist makeCPU >>= writeCXX "/tmp/cpu"
