import Blarney
import Blarney.BitPat
import System.Process
import System.Environment

-- Tiny 8-bit CPU specification
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00ZZNNNN  | Write value 0000NNNN to register ZZ
-- 01ZZXXYY  | Add register XX to register YY and store in register ZZ
-- 10NNNNYY  | Branch back by NNNN instructions if YY is non-zero
-- 11NNNNNN  | Halt

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
        regFile.update rd (zeroExtend imm)
        pc <== pc.val + 1
        display "rf[" rd "] := 0x" (formatHex 2 (zeroExtend imm :: Bit 8))

  -- Add instruction
  let add rd rs0 rs1 = do
        let sum = regFile!rs0 + regFile!rs1
        regFile.update rd sum
        pc <== pc.val + 1
        display "rf[" rd "] := 0x" (formatHex 2 sum)

  -- Branch instruction
  let bnz offset rs = do
        if regFile!rs .==. 0
          then pc <== pc.val + 1
          else pc <== pc.val - zeroExtend offset

  -- Halt instruction
  let halt imm = finish

  always do
    -- Fetch
    when fetch.val do
      instrMem.load pc.val
      fetch <== 0

    -- Execute
    when (inv fetch.val) do
      match instrMem.out
        [
          literal 0b00 <#> variable @2 <#> variable @4 ==>  li,
          literal 0b01 <#> variable @2 <#> variable @2 <#> variable @2 ==> add,
          literal 0b10 <#> variable @4 <#> variable @2 ==>  bnz,
          literal 0b11 <#> variable @6 ==>  halt
        ]
      fetch <== 1

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate makeCPUSpec
     | otherwise -> writeVerilogTop makeCPUSpec "Spec" "Spec-Verilog/"
