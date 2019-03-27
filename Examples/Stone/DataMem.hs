-- 32-bit wide tightly-coupled data memory
module DataMem where

import Blarney
import Blarney.RAM

-- Data memory size in bytes
type LogDataMemSize = 16

-- Implement data memory as four block RAMs
-- (one for each byte of word)
type DataMem = [RAM (Bit (LogDataMemSize-2)) (Bit 8)]

-- Constructor
makeDataMem :: Module DataMem
makeDataMem = sequence [makeRAM | i <- [0..3]]

-- RV32I memory access width
type AccessWidth = Bit 2

-- Byte, half-word, or word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- Determine byte enables given access width and address
genByteEnable :: AccessWidth -> Bit 32 -> [Bit 1]
genByteEnable w addr =
  selectList [
    isWordAccess w --> [1, 1, 1, 1]
  , isHalfAccess w --> [a.==.2, a.==.2, a.==.0, a.==.0]
  , isByteAccess w --> [a.==.3, a.==.2, a.==.1, a.==.0]
  ]
  where a :: Bit 2 = truncate addr

-- Align a write using access width
writeAlign :: AccessWidth -> Bit 32 -> [Bit 8]
writeAlign w d =
  selectList [
    isWordAccess w --> [b3, b2, b1, b0]
  , isHalfAccess w --> [b1, b0, b1, b0]
  , isByteAccess w --> [b0, b0, b0, b0]
  ]
  where
    b0 = range @7 @0 d
    b1 = range @15 @8 d
    b2 = range @23 @16 d
    b3 = range @31 @24 d

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
