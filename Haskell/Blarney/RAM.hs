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

-- True dual port RAM module
makeTrueDualRAMCore :: (Bits a, Bits d) =>
                       Maybe String -> RTL (RAM a d, RAM a d)
makeTrueDualRAMCore init = do
  -- Address bus and data bus and write-enable
  addrBusA :: Wire a <- makeWire
  dataBusA :: Wire d <- makeWire
  writeEnA :: Wire (Bit 1) <- makeWireDefault 0

  addrBusB :: Wire a <- makeWire
  dataBusB :: Wire d <- makeWire
  writeEnB :: Wire (Bit 1) <- makeWireDefault 0

  -- RAM primitive
  let ramPrim = case init of
        Nothing  -> ramTrueDual
        Just str -> ramTrueDualInit str
 
  let (outA, outB) = ramPrim (pack (val addrBusA),
                                pack (val dataBusA),
                                  val writeEnA)
                             (pack (val addrBusB),
                                pack (val dataBusB),
                                  val writeEnB)

  let outA' = reg 0 outA
  let outB' = reg 0 outB

  -- Methods
  let loadA a = do
        addrBusA <== a

  let storeA a d = do
        addrBusA <== a
        dataBusA <== d
        writeEnA <== 1

  let loadB a = do
        addrBusB <== a

  let storeB a d = do
        addrBusB <== a
        dataBusB <== d
        writeEnB <== 1

  -- Return interface
  return (RAM loadA storeA (unpack outA) (unpack outA'),
          RAM loadB storeB (unpack outB) (unpack outB'))

makeTrueDualRAMInit :: (Bits a, Bits d) => String -> RTL (RAM a d, RAM a d)
makeTrueDualRAMInit init = makeTrueDualRAMCore (Just init)

makeTrueDualRAM :: (Bits a, Bits d) => RTL (RAM a d, RAM a d)
makeTrueDualRAM = makeTrueDualRAMCore Nothing

