-- Tiny 8-bit CPU specification
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00ZZNNNN  | Write value 0000NNNN to register ZZ
-- 01ZZXXYY  | Add register XX to register YY and store in register ZZ
-- 10NNNNYY  | Branch back by NNNN instructions if YY is non-zero
-- 11NNNNNN  | Halt

import Blarney
import Blarney.BitPat
import System.Process

makeCPUSpec :: Module ()
makeCPUSpec = do
  -- Instruction memory (containing 32 instructions)
  instrMem :: RAM (Bit 5) (Bit 8) <- makeRAMInit "instrs.hex"

  -- Register file (containing 4 registers)
  regFile :: RegFile (Bit 2) (Bit 8) <- makeRegFile

  -- Program counter
  pc :: Reg (Bit 5) <- makeReg 0

  -- Are we fetching (1) or executing (0)
  fetch :: Reg (Bit 1) <- makeReg 1

  -- Load immediate instruction
  let li rd imm = do
        update regFile rd (zeroExtend imm)
        pc <== pc.val + 1
        display "rf[%0d]" rd " := 0x%02x" (zeroExtend imm :: Bit 8)

  -- Add instruction
  let add rd rs0 rs1 = do
        let sum = regFile!rs0 + regFile!rs1
        update regFile rd sum
        pc <== pc.val + 1
        display "rf[%0d]" rd " := 0x%02x" sum

  -- Branch instruction
  let bnz offset rs = do
        if regFile!rs .==. 0
          then pc <== pc.val + 1
          else pc <== pc.val - zeroExtend offset

  -- Halt instruction
  let halt imm = finish

  always do
    -- Fetch
    when (fetch.val) $ do
      load instrMem (pc.val)
      fetch <== 0

    -- Execute
    when (fetch.val.inv) $ do
      match (instrMem.out)
        [
          lit 0b00 <#> var @2 <#> var @4              ==>  li,
          lit 0b01 <#> var @2 <#> var @2  <#> var @2  ==>  add,
          lit 0b10 <#> var @4 <#> var @2              ==>  bnz,
          lit 0b11 <#> var @6                         ==>  halt
        ]
      fetch <== 1

main :: IO ()
main = do
  writeVerilogTop makeCPUSpec "top" "Spec-Verilog/"
  system "cp instrs.hex Spec-Verilog/"
  return ()
