import Blarney
import Blarney.BitScan
import System.Environment

-- Semantics of add instruction
sem_add :: Bit 5 -> Bit 5 -> Bit 5 -> Action ()
sem_add rs2 rs1 rd = display "add r" rd ", r" rs1 ", r" rs2

-- Semantics of addi instruction
sem_addi :: Bit 12 -> Bit 5 -> Bit 5 -> Action ()
sem_addi imm rs1 rd = display "addi r" rd ", r" rs1 ", " imm

-- Semantics of store instruciton
sem_sw :: Bit 12 -> Bit 5 -> Bit 5 -> Action ()
sem_sw imm rs2 rs1 = display "sw r" rs2 ", " imm "(r" rs1 ")"

top :: Module ()
top = always do
  let instr :: Bit 32 = 0b1000000_00001_00010_010_00001_0100011

  match instr
    [
      "0000000   rs2[4:0]  rs1[4:0] 000 rd[4:0]  0110011" ==> sem_add,
      "          imm[11:0] rs1[4:0] 000 rd[4:0]  0010011" ==> sem_addi,
      "imm[11:5] rs2[4:0]  rs1[4:0] 010 imm[4:0] 0100011" ==> sem_sw
    ]

  finish

-- Main function
main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "BitScan" "BitScan-Verilog/"
