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
  instrMem :: RAM (Bit 5) (Bit 8) <- makeRAMInit "instrs.hex"

  -- Register file
  regFile :: RegFile (Bit 2) (Bit 8) <- makeRegFile

  -- Program counter
  pc :: Reg (Bit 5) <- makeRegInit 0

  -- Are we fetching (1) or executing (0)
  fetch :: Reg (Bit 1) <- makeRegInit 1

  -- Load immediate instruction
  let li rd imm = do
        update regFile rd (zeroExtend imm)
        pc <== pc.val + 1
        display "rf[" rd "] := " imm

  -- Add instruction
  let add rd rs0 rs1 = do
        let sum = regFile!rs0 + regFile!rs1
        update regFile rd sum
        pc <== pc.val + 1
        display "rf[" rd "] := " sum

  -- Branch instruction
  let bnz offset rs = do
        if regFile!rs .==. 0
          then pc <== pc.val + 1
          else pc <== pc.val - zeroExtend offset

  -- Halt instruction
  let halt imm = finish

  -- Fetch
  when (fetch.val) $ do
    load instrMem (pc.val)
    fetch <== 0

  -- Execute
  when (fetch.val.inv) $ do
    match (instrMem.out)
      [
        Var(2) <> Var(4)           <> Lit(2,0b00) ==> li,
        Var(2) <> Var(2) <> Var(2) <> Lit(2,0b01) ==> add,
        Var(4) <>           Var(2) <> Lit(2,0b10) ==> bnz,
        Var(6) <>                     Lit(2,0b11) ==> halt
      ]
    fetch <== 1

main :: IO ()
main = netlist makeCPUSpec >>= writeCXX "/tmp/spec"
