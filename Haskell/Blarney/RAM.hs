module Blarney.RAM where

import Prelude
import Blarney.RTL
import Blarney.Bits
import Blarney.Bit
import Blarney.Prelude

-- RAM interface
data RAM a d =
  RAM {
    load :: a -> RTL ()
  , store :: a -> d -> RTL ()
  , out :: d
  , out' :: d
  , writeEn :: Bit 1
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
  return (RAM load store (unpack output) (unpack output') (val writeEn))

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
  return (RAM loadA storeA (unpack outA) (unpack outA') (val writeEnA),
          RAM loadB storeB (unpack outB) (unpack outB') (val writeEnB))

makeTrueDualRAMInit :: (Bits a, Bits d) => String -> RTL (RAM a d, RAM a d)
makeTrueDualRAMInit init = makeTrueDualRAMCore (Just init)

makeTrueDualRAM :: (Bits a, Bits d) => RTL (RAM a d, RAM a d)
makeTrueDualRAM = makeTrueDualRAMCore Nothing

-- Dual port RAM module
-- (One port used for reading and the other for writing)
makeDualRAMCore :: (Bits a, Bits d) => Maybe String -> RTL (RAM a d)
makeDualRAMCore init = do
  -- Create true dual port RAM
  (portA :: RAM a d, portB :: RAM a d) <- makeTrueDualRAMCore init

  -- Methods
  let loadA a = load portA a

  let storeB a d = store portB a d

  -- Return interface
  return (RAM loadA storeB (out portA) (out' portA) (writeEn portB))

makeDualRAMInit :: (Bits a, Bits d) => String -> RTL (RAM a d)
makeDualRAMInit init = makeDualRAMCore (Just init)

makeDualRAM :: (Bits a, Bits d) => RTL (RAM a d)
makeDualRAM = makeDualRAMCore Nothing

-- Dual port RAM module with pass-through
-- (Read and write to same address yields new data)
makeDualRAMPassthroughCore :: (Bits a, Bits d) => Maybe String -> RTL (RAM a d)
makeDualRAMPassthroughCore init = do
  -- Create dual port RAM
  ram :: RAM a d <- makeDualRAMCore init

  -- Details of lastest load and store
  la :: Wire a <- makeWire
  sa :: Wire a <- makeWire
  sd :: Wire d <- makeWire

  -- Methods
  let loadMethod a = do
        load ram a
        la <== a

  let storeMethod a d = do
        store ram a d
        sa <== a
        sd <== d

  let outMethod =
        (active' la .&. active' sa .&.
           (val' la .==. val' sa)) ?
              (val' sd, out ram)

  let outMethod' = unpack (reg 0 (pack outMethod))

  return (RAM loadMethod storeMethod outMethod outMethod' (writeEn ram))

makeDualRAMPassthroughInit :: (Bits a, Bits d) => String -> RTL (RAM a d)
makeDualRAMPassthroughInit init = makeDualRAMPassthroughCore (Just init)

makeDualRAMPassthrough :: (Bits a, Bits d) => RTL (RAM a d)
makeDualRAMPassthrough = makeDualRAMPassthroughCore Nothing
