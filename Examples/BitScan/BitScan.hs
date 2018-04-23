import Blarney
import Blarney.BitScan

-- Semantics of add instruction
add :: Bit 5 -> Bit 5 -> Bit 5 -> RTL ()
add rs2 rs1 rd = display "add " rd ", " rs1 ", " rs2

-- Semantics of addi instruction
addi :: Bit 12 -> Bit 5 -> Bit 5 -> RTL ()
addi imm rs1 rd = display "addi " rd ", " rs1 ", " imm

-- Semantics of store instruciton
sw :: Bit 12 -> Bit 5 -> Bit 5 -> RTL ()
sw imm rs2 rs1 = display "sw " rs2 ", " rs1 "[" imm "]"

top :: RTL ()
top = do
  let instr :: Bit 32 = 0b10000000000100010010000010100011

  match instr
    [
      "0000000   rs2[4:0]  rs1[4:0] 000 rd[4:0]  0110011" ==> add,
      "          imm[11:0] rs1[4:0] 000 rd[4:0]  0010011" ==> addi,
      "imm[11:5] rs2[4:0]  rs1[4:0] 010 imm[4:0] 0100011" ==> sw
    ]

-- Main function
main :: IO ()
main = netlist top >>= writeCXX "/tmp/bitscan"
