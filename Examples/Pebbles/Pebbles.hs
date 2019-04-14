{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Pebbles where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.BitScan
import Blarney.PulseWire

-- Pebbles imports
import CSR
import DataMem
import Pipeline

addi :: State -> Bit 12 -> Action ()
addi s imm = s.result <== s.opA + signExtend imm

slti :: State -> Bit 12 -> Action ()
slti s imm = s.result <== (s.opA `sLT` signExtend imm) ? (1, 0)

sltiu :: State -> Bit 12 -> Action ()
sltiu s imm = s.result <== (s.opA .<. signExtend imm) ? (1, 0)

andi :: State -> Bit 12 -> Action ()
andi s imm = s.result <== s.opA .&. signExtend imm

ori :: State -> Bit 12 -> Action ()
ori s imm = s.result <== s.opA .|. signExtend imm

xori :: State -> Bit 12 -> Action ()
xori s imm = s.result <== s.opA .^. signExtend imm

slli :: State -> Bit 5 -> Action ()
slli s imm = s.result <== s.opA .<<. imm

srli :: State -> Bit 5 -> Action ()
srli s imm = s.result <== s.opA .>>. imm

srai :: State -> Bit 5 -> Action ()
srai s imm = s.result <== s.opA .>>>. imm

lui :: State -> Bit 20 -> Action ()
lui s imm = s.result <== signExtend (imm # (0 :: Bit 12))

auipc :: State -> Bit 20 -> Action ()
auipc s imm = s.result <== s.pc.val + (imm # (0 :: Bit 12))

add :: State -> Action ()
add s = s.result <== s.opA + s.opB

slt :: State -> Action ()
slt s = s.result <== (s.opA `sLT` s.opB) ? (1, 0)

sltu :: State -> Action ()
sltu s = s.result <== (s.opA .<. s.opB) ? (1, 0)

and' :: State -> Action ()
and' s = s.result <== s.opA .&. s.opB

or' :: State -> Action ()
or' s = s.result <== s.opA .|. s.opB

xor :: State -> Action ()
xor s = s.result <== s.opA .^. s.opB

sll :: State -> Action ()
sll s = s.result <== s.opA .<<. range @4 @0 (s.opB)

srl :: State -> Action ()
srl s = s.result <== s.opA .>>. range @4 @0 (s.opB)

sub :: State -> Action ()
sub s = s.result <== s.opA - s.opB

sra :: State -> Action ()
sra s = s.result <== s.opA .>>>. range @4 @0 (s.opB)

jal :: State -> Bit 20 -> Action ()
jal s imm = do
  s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))
  s.result <== s.pc.val + 4

jalr :: State -> Bit 12 -> Action ()
jalr s imm = do
  s.pc <== truncateLSB (s.opA + signExtend imm) # (0 :: Bit 1)
  s.result <== s.pc.val + 4

beq :: State -> Bit 12 -> Action ()
beq s imm = do
  when (s.opA .==. s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

bne :: State -> Bit 12 -> Action ()
bne s imm = do
  when (s.opA .!=. s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

blt :: State -> Bit 12 -> Action ()
blt s imm = do
  when (s.opA `sLT` s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

bltu :: State -> Bit 12 -> Action ()
bltu s imm = do
  when (s.opA .<. s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

bge :: State -> Bit 12 -> Action ()
bge s imm = do
  when (s.opA `sGTE` s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

bgeu :: State -> Bit 12 -> Action ()
bgeu s imm = do
  when (s.opA .>=. s.opB) do
    s.pc <== s.pc.val + signExtend (imm # (0 :: Bit 1))

memRead_0 :: State -> DataMem -> Bit 12 -> Action ()
memRead_0 State{opA = a, lateResult = lr} mem imm = do
  dataMemRead mem (a + signExtend imm)
  lr.pulse

memRead_1 :: State -> DataMem -> Bit 12 -> Bit 1 -> Bit 2 -> Action ()
memRead_1 s mem imm unsigned width = do
  let tmp = readMux mem (s.opA + signExtend imm) width unsigned
  s.result <== tmp
  let isDebug = testPlusArgs "DEBUG"
  when isDebug $ display "----- MEM READ @ 0x%0x, data: 0x%0x" (s.opA + signExtend imm) tmp

memWrite :: State -> DataMem -> Bit 12 -> Bit 2 -> Action ()
memWrite s mem imm width = do
  dataMemWrite mem width (s.opA + signExtend imm) (s.opB)
  let isDebug = testPlusArgs "DEBUG"
  when isDebug $ display "----- MEM WRITE @ 0x%0x, data: 0x%0x" (s.opA + signExtend imm) (s.opB)

fence :: State -> Bit 4 -> Bit 4 -> Bit 4 -> Action ()
fence s fm pred succ = display "fence not implemented"

ecall :: State -> Action ()
ecall s = display "ecall not implemented"

ebreak :: State -> Action ()
ebreak s = display "ebreak not implemented"

csrrw :: State -> CSRUnit -> Bit 12 -> Action ()
csrrw s csrUnit csr = do
  readCSR csrUnit csr (s.result)
  writeCSR csrUnit csr (s.opA)

-- RV32I CPU, with UART input and output channels
makePebbles :: Stream (Bit 8) -> Module (Stream (Bit 8))
makePebbles uartIn = do
  -- Tightly-coupled data memory
  mem <- makeDataMem

  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- Execute rules
  let execute s =
        [ "imm[11:0] <5> 000 <5> 0010011" ==> addi s
        , "imm[11:0] <5> 010 <5> 0010011" ==> slti s
        , "imm[11:0] <5> 011 <5> 0010011" ==> sltiu s
        , "imm[11:0] <5> 111 <5> 0010011" ==> andi s
        , "imm[11:0] <5> 110 <5> 0010011" ==> ori s
        , "imm[11:0] <5> 100 <5> 0010011" ==> xori s
        , "0000000 imm[4:0] <5> 001 <5> 0010011" ==> slli s
        , "0000000 imm[4:0] <5> 101 <5> 0010011" ==> srli s
        , "0100000 imm[4:0] <5> 101 <5> 0010011" ==> srai s
        , "imm[19:0] <5> 0110111" ==> lui s
        , "imm[19:0] <5> 0010111" ==> auipc s
        , "0000000 <5> <5> 000 <5> 0110011" ==> add s
        , "0000000 <5> <5> 010 <5> 0110011" ==> slt s
        , "0000000 <5> <5> 011 <5> 0110011" ==> sltu s
        , "0000000 <5> <5> 111 <5> 0110011" ==> and' s
        , "0000000 <5> <5> 110 <5> 0110011" ==> or' s
        , "0000000 <5> <5> 100 <5> 0110011" ==> xor s
        , "0000000 <5> <5> 001 <5> 0110011" ==> sll s
        , "0000000 <5> <5> 101 <5> 0110011" ==> srl s
        , "0100000 <5> <5> 000 <5> 0110011" ==> sub s
        , "0100000 <5> <5> 101 <5> 0110011" ==> sra s
        , "imm[19] imm[9:0] imm[10] imm[18:11] <5> 1101111" ==> jal s
        , "imm[11:0] <5> 000 <5> 1100111" ==> jalr s
        , "imm[11] imm[9:4] <5> <5> 000 imm[3:0] imm[10] 1100011" ==> beq s
        , "imm[11] imm[9:4] <5> <5> 001 imm[3:0] imm[10] 1100011" ==> bne s
        , "imm[11] imm[9:4] <5> <5> 100 imm[3:0] imm[10] 1100011" ==> blt s
        , "imm[11] imm[9:4] <5> <5> 110 imm[3:0] imm[10] 1100011" ==> bltu s
        , "imm[11] imm[9:4] <5> <5> 101 imm[3:0] imm[10] 1100011" ==> bge s
        , "imm[11] imm[9:4] <5> <5> 111 imm[3:0] imm[10] 1100011" ==> bgeu s
        , "imm[11:0] <5> <3> <5> 0000011" ==> memRead_0 s mem
        , "imm[11:5] <5> <5> 0 w<2> imm[4:0] 0100011" ==> memWrite s mem
        , "fm[3:0] pred[3:0] succ[3:0] <5> 000 <5> 0001111" ==> fence s
        , "000000000000 <5> 000 <5> 1110011" ==> ecall s
        , "000000000001 <5> 000 <5> 1110011" ==> ebreak s
        , "csr<12> <5> 001 <5> 1110011" ==> csrrw s csrUnit
        ]

  -- Write-back rules
  let writeBack s =
        [ "imm[11:0] <5> u<1> w<2> <5> 0000011" ==> memRead_1 s mem
        ]

  -- CPU pipeline
  makeCPUPipeline $
    Config {
      srcA = range @19 @15
    , srcB = range @24 @20
    , dst  = range @11 @7
    , writeBackRules = writeBack
    , execRules = execute
    }

  return uartOut
