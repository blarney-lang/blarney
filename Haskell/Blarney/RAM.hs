{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Blarney.RAM where

import Blarney.RTL
import Blarney.Bits
import Blarney.Bit

-- RAM interface
data RAM a d =
  RAM {
    load     :: a -> RTL ()
  , store    :: a -> d -> RTL ()
  , out      :: d
  , out'     :: d
  }

-- RAM module
makeRAMCore :: (Bits a, Bits d) => Maybe String -> RTL (RAM a d)
makeRAMCore init = do
  -- Address bus and data bus and write-enable
  addrBus :: Wire a <- makeWire
  dataBus :: Wire d <- makeWire
  writeEn :: Wire (Bit 1) <- makeWireDefault 0

  -- RAM primitive
  let ramPrim = case init of { Nothing -> ram ; Just str -> ramInit str }
  let output = ramPrim (pack (val addrBus),
                          pack (val dataBus),
                            val writeEn)
  let output' = reg 0 output

  -- Methods
  let load a = do
        addrBus <== a

  let store a d = do
        addrBus <== a
        dataBus <== d
        writeEn <== 1

  -- Return interface
  return (RAM load store (unpack output) (unpack output'))

makeRAMInit :: (Bits a, Bits d) => String -> RTL (RAM a d)
makeRAMInit init = makeRAMCore (Just init)

makeRAM :: (Bits a, Bits d) => RTL (RAM a d)
makeRAM = makeRAMCore Nothing
