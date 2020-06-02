module Pebbles where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.BitScan

-- Pebbles imports
import CSR
import Trap
import DataMem
import Pipeline

-- RISCV I decode
-- ==============

decode =
  [ "imm[31:12] <5> 0110111" --> "LUI"
  , "imm[31:12] <5> 0010111" --> "AUIPC"
  , "imm[11:0] <5> 000 <5> 0010011" --> "ADD"
  , "imm[11:0] <5> 010 <5> 0010011" --> "SLT"
  , "imm[11:0] <5> 011 <5> 0010011" --> "SLTU"
  , "imm[11:0] <5> 111 <5> 0010011" --> "AND"
  , "imm[11:0] <5> 110 <5> 0010011" --> "OR"
  , "imm[11:0] <5> 100 <5> 0010011" --> "XOR"
  , "0000000 imm[4:0] <5> 001 <5> 0010011" --> "SLL"
  , "0000000 imm[4:0] <5> 101 <5> 0010011" --> "SRL"
  , "0100000 imm[4:0] <5> 101 <5> 0010011" --> "SRA"
  , "0000000 <5> <5> 000 <5> 0110011" --> "ADD"
  , "0000000 <5> <5> 010 <5> 0110011" --> "SLT"
  , "0000000 <5> <5> 011 <5> 0110011" --> "SLTU"
  , "0000000 <5> <5> 111 <5> 0110011" --> "AND"
  , "0000000 <5> <5> 110 <5> 0110011" --> "OR"
  , "0000000 <5> <5> 100 <5> 0110011" --> "XOR"
  , "0100000 <5> <5> 000 <5> 0110011" --> "SUB"
  , "0000000 <5> <5> 001 <5> 0110011" --> "SLL"
  , "0000000 <5> <5> 101 <5> 0110011" --> "SRL"
  , "0100000 <5> <5> 101 <5> 0110011" --> "SRA"
  , "imm[20] imm[10:1] imm[11] imm[19:12] <5> 1101111" --> "JAL"
  , "imm[11:0] <5> 000 <5> 1100111" --> "JALR"
  , "off[12] off[10:5] <5> <5> 000 off[4:1] off[11] 1100011" --> "BEQ"
  , "off[12] off[10:5] <5> <5> 001 off[4:1] off[11] 1100011" --> "BNE"
  , "off[12] off[10:5] <5> <5> 100 off[4:1] off[11] 1100011" --> "BLT"
  , "off[12] off[10:5] <5> <5> 110 off[4:1] off[11] 1100011" --> "BLTU"
  , "off[12] off[10:5] <5> <5> 101 off[4:1] off[11] 1100011" --> "BGE"
  , "off[12] off[10:5] <5> <5> 111 off[4:1] off[11] 1100011" --> "BGEU"
  , "imm[11:0] <5> <3> <5> 0000011" --> "LOAD"
  , "imm[11:5] <5> <5> 0 <2> imm[4:0] 0100011" --> "STORE"
  , "<4> <4> <4> <5> 000 <5> 0001111" --> "FENCE"
  , "000000000000 <5> 000 <5> 1110011" --> "ECALL"
  , "000000000001 <5> 000 <5> 1110011" --> "EBREAK"
  , "imm[11:0] <5> 001 <5> 1110011" --> "CSRRW"
  ]

-- Determine the access width of a load/store
getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = slice @13 @12

-- Determine if the load is unsigned
isUnsignedLoad :: Bit 32 -> Bit 1
isUnsignedLoad = at @14

-- RISCV I pre-execute
-- ===================

preExecute :: State -> Action ()
preExecute s = do
  -- Signal late result on a load
  when (s.opcode `is` ["LOAD"]) do
    s.late <== true

-- RISCV I execute
-- ===============

execute :: CSRUnit -> DataMem -> State -> Action ()
execute csrUnit mem s = do
  -- 33-bit add/sub/compare
  let uns = s.opcode `is` ["SLTU", "BLTU", "BGEU"]
  let addA = (uns ? (0, at @31 (s.opA))) # s.opA
  let addB = (uns ? (0, at @31 (s.opBorImm))) # s.opBorImm
  let isAdd = s.opcode `is` ["ADD"]
  let sum = addA + (isAdd ? (addB, inv addB))
                 + (isAdd ? (0, 1))
  let less = at @32 sum
  let equal = s.opA .==. s.opBorImm

  when (s.opcode `is` ["ADD", "SUB"]) do
    s.result <== truncate sum

  when (s.opcode `is` ["SLT", "SLTU"]) do
    s.result <== zeroExtend less

  when (s.opcode `is` ["AND"]) do
    s.result <== s.opA .&. s.opBorImm

  when (s.opcode `is` ["OR"]) do
    s.result <== s.opA .|. s.opBorImm

  when (s.opcode `is` ["XOR"]) do
    s.result <== s.opA .^. s.opBorImm

  when (s.opcode `is` ["LUI"]) do
    s.result <== s.opBorImm

  when (s.opcode `is` ["AUIPC"]) do
    s.result <== s.pc.val + s.opBorImm

  when (s.opcode `is` ["SLL"]) do
    s.result <== s.opA .<<. slice @4 @0 (s.opBorImm)

  when (s.opcode `is` ["SRL", "SRA"]) do
    let ext = s.opcode `is` ["SRA"] ? (at @31 (s.opA), 0)
    let opAExt = ext # (s.opA)
    s.result <== truncate (opAExt .>>>. slice @4 @0 (s.opBorImm))

  let branch =
        orList [
          s.opcode `is` ["BEQ"] .&. equal
        , s.opcode `is` ["BNE"] .&. inv equal
        , s.opcode `is` ["BLT", "BLTU"] .&. less
        , s.opcode `is` ["BGE", "BGEU"] .&. inv less
        ]

  when branch do
    let offset = getField (s.fields) "off"
    s.pc <== s.pc.val + offset.val

  when (s.opcode `is` ["JAL"]) do
    s.pc <== s.pc.val + s.opBorImm -- TODO: share with AUIPC?

  when (s.opcode `is` ["JALR"]) do
    s.pc <== truncateLSB (s.opA + s.opBorImm) # (0 :: Bit 1)
    --TODO: s.pc <== truncateLSB sum # (0 :: Bit 1)

  when (s.opcode `is` ["JAL", "JALR"]) do
    s.result <== s.pc.val + 4

  let addr = s.opA + s.opBorImm

  when (s.opcode `is` ["LOAD"]) do
    dataMemRead mem addr

  when (s.opcode `is` ["STORE"]) do
    dataMemWrite mem (s.instr.getAccessWidth) addr (s.opB)

  when (s.opcode `is` ["FENCE"]) do
    display "fence not implemented"

  when (s.opcode `is` ["ECALL"]) do
    trap s csrUnit (Exception exc_eCallFromU)

  when (s.opcode `is` ["EBREAK"]) do
    trap s csrUnit (Exception exc_breakpoint)

  when (s.opcode `is` ["CSRRW"]) do
    readCSR csrUnit (s.opBorImm.truncate) (s.result)
    writeCSR csrUnit (s.opBorImm.truncate) (s.opA)

-- RISCV I post-execute
-- ====================

postExecute :: DataMem -> State -> Action ()
postExecute mem s = do
  when (s.opcode `is` ["LOAD"]) do
    s.result <== readMux mem (s.opA + s.opBorImm)
                   (s.instr.getAccessWidth) (s.instr.isUnsignedLoad)

-- RV32I CPU, with UART input and output channels
makePebbles :: Bool -> Stream (Bit 8) -> Module (Stream (Bit 8))
makePebbles sim uartIn = do
  -- Tightly-coupled data memory
  mem <- makeDataMem sim

  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- CPU pipeline
  makeCPUPipeline sim $
    Config {
      srcA = slice @19 @15
    , srcB = slice @24 @20
    , dst = slice @11 @7
    , decodeTable = decode
    , preExecRules = preExecute
    , execRules = execute csrUnit mem
    , postExecRules = postExecute mem
    }

  return uartOut
