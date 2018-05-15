-- Tiny 8-bit CPU specification
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- ZZNNNN00  | Write value 0000NNNN to register ZZ
-- ZZXXYY01  | Add register XX to register YY and store in register ZZ
-- NNNNYY10  | Branch back by NNNN instructions if YY is non-zero
-- NNNNNN11  | Halt

import Blarney
import Blarney.RAM
import Blarney.RegFile
import Blarney.BitPat

#include "BitPat.h"

makeCPUSpec :: RTL ()
makeCPUSpec = do
  -- Instruction memory
  -- TODO implement makeRegArray -- Verilog array of regs
  -- instead of makeRegFile?
  --instrMem :: RAM (Bit 8) (Bit 8) <- makeRAMInit "instrs.hex"
  instrMem :: RegFile (Bit 5) (Bit 8) <- makeRegFile

  -- Register file
  regFile :: RegFile (Bit 2) (Bit 8) <- makeRegFile

  -- Program counter
  pc :: Reg (Bit 5) <- makeRegInit 0

  -- Load immediate instruction
  let li rd imm = do
        update regFile rd (zeroExtend imm)
        pc <== pc.val + 1

  -- Add instruction
  let add rd rs0 rs1 = do
        update regFile rd (regFile!rs0 + regFile!rs1)
        pc <== pc.val + 1

  -- Branch instruction
  let bnz offset rs = do
        if regFile!rs .==. 0
          then pc <== pc.val + 1
          else pc <== pc.val - zeroExtend offset

  -- Halt instruction
  let halt imm = finish

  -- Instruction dispatch
  match (instrMem!(pc.val))
    [
      Var(2) <> Var(4)           <> Lit(2,0b01) ==> li,
      Var(2) <> Var(2) <> Var(2) <> Lit(2,0b01) ==> add,
      Var(4) <>           Var(2) <> Lit(2,0b10) ==> bnz,
      Var(6) <>                     Lit(2,0b11) ==> halt
    ]

main :: IO ()
main = return ()
