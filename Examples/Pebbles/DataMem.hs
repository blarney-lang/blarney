-- 32-bit wide tightly-coupled data memory
module DataMem where

import Blarney

-- Data memory size in bytes
type LogDataMemSize = 16

-- Implement data memory as four block RAMs
-- (one for each byte of word)
type DataMem = [RAM (Bit (LogDataMemSize-2)) (Bit 8)]

-- Constructor
makeDataMem :: Bool -> Module DataMem
makeDataMem sim =
    sequence [makeRAMInit ("data_" ++ show i ++ ext) | i <- [0..3]]
  where ext = if sim then ".hex" else ".mif"

-- RV32I memory access width
type AccessWidth = Bit 2

-- Byte, half-word, or word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- ======================
-- Writing to data memory
-- ======================

-- Determine byte enables given access width and address
genByteEnable :: AccessWidth -> Bit 32 -> [Bit 1]
genByteEnable w addr =
  selectList [
    isWordAccess w --> [1, 1, 1, 1]
  , isHalfAccess w --> [a.==.0, a.==.0, a.==.2, a.==.2]
  , isByteAccess w --> [a.==.0, a.==.1, a.==.2, a.==.3]
  ]
  where a :: Bit 2 = truncate addr

-- Align a write using access width
writeAlign :: AccessWidth -> Bit 32 -> [Bit 8]
writeAlign w d =
  selectList [
    isWordAccess w --> [b0, b1, b2, b3]
  , isHalfAccess w --> [b0, b1, b0, b1]
  , isByteAccess w --> [b0, b0, b0, b0]
  ]
  where
    b0 = slice @7 @0 d
    b1 = slice @15 @8 d
    b2 = slice @23 @16 d
    b3 = slice @31 @24 d

-- Write to data memory
dataMemWrite :: DataMem -> AccessWidth -> Bit 32 -> Bit 32 -> Action ()
dataMemWrite dataMem w addr d =
  sequence_
    [ when byteEn do
        let writeAddr = lower (upper addr :: Bit 30)
        store mem writeAddr byte
    | (mem, byte, byteEn) <- zip3 dataMem bytes byteEns
    ]
  where
    bytes = writeAlign w d
    byteEns = genByteEnable w addr

-- ========================
-- Reading from data memory
-- ========================

-- Process output of data memory using low address bits,
-- access width, and unsigned flag.
readMux :: DataMem -> Bit 32 -> AccessWidth -> Bit 1 -> Bit 32
readMux dataMem addr w isUnsigned =
    select [
      isWordAccess w --> b3 # b2 # b1 # b0
    , isHalfAccess w --> hExt # h
    , isByteAccess w --> bExt # b
    ]
  where
    a = lower addr :: Bit 2
    b = select [
          a .==. 0 --> b0
        , a .==. 1 --> b1
        , a .==. 2 --> b2
        , a .==. 3 --> b3
        ]
    h = (at @1 a .==. 0) ? (b1 # b0, b3 # b2)
    bExt = isUnsigned ? (0, signExtend (at @7 b))
    hExt = isUnsigned ? (0, signExtend (at @15 h))
    [b0, b1, b2, b3] = map out dataMem

-- Read from data memory
dataMemRead :: DataMem -> Bit 32 -> Action ()
dataMemRead dataMem addr =
    sequence_ [load mem a | mem <- dataMem]
  where a = lower (upper addr :: Bit 30)
