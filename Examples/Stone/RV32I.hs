import Blarney
import Blarney.RAM
import Blarney.BitScan
import Pipeline

add :: State -> Action ()
add s = s.result <== s.opA + s.opB

addi :: State -> Bit 12 -> Action ()
addi s imm = s.result <== s.opA + signExtend imm

bne :: State -> Bit 12 -> Action ()
bne s imm = do
  when (s.opA .!=. s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

makeRV32I :: Module ()
makeRV32I = do
  let execute s =
        [ "imm[11:0] _rs1[4:0] 000 _rd[4:0] 0010011" ==> addi s
        , "0000000 _rs2[4:0] _rs1[4:0] 000 _rd[4:0] 0110011" ==> add s
        , "imm[11] imm[9:4] _rs2[4:0] _rs1[4:0] 001" ++
          "imm[3:0] imm[10] 1100011" ==> bne s
        ]

  makeCPUPipeline $
    Config {
      srcA = range @19 @15
    , srcB = range @24 @20
    , dst  = range @11 @7
    , preExecRules = \s -> []
    , execRules = execute
    }

-- Main function
main :: IO ()
main = do
  writeVerilogTop makeRV32I "top" "RV32I-Verilog/"
  return ()
