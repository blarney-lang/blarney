import Blarney
import Blarney.BitPat

-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00ZZNNNN  | Write value 0000NNNN to register ZZ
-- 01ZZXXYY  | Add register XX to register YY and store in register ZZ
-- 10NNNNYY  | Branch back by NNNN instructions if YY is non-zero
-- 11NNNNNN  | Halt

-- Instruction dispatch
top :: Module ()
top = always do
  -- Sample add instruction
  let instr :: Bit 8 = 0b01_00_01_10

  -- Dispatch
  match instr
    [
      lit @2 0b00 <#> var @2 <#> var @4
        ==>
      \z n -> display "li %0d, %0d" z n

    , lit @2 0b01 <#> var @2 <#> var @2 <#> var @2
        ==>
      \z x y -> display "add %0d, %0d, %0d" z x y

    , lit @2 0b10 <#> var @4 <#> var @2
        ==>
      \n y -> display "bnz %0d, 0x%0x" y n

    , lit @2 0b11 <#> var @6
        ==>
      \n -> display "halt" >> finish
    ]

  finish

-- Main function
main :: IO ()
main = writeVerilogTop top "top" "BitPat-Verilog/"
