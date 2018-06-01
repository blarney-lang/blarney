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
  let ramPrimitive = case init of
        Nothing  -> ram
        Just str -> ramInit str

  -- RAM output
  let output = ramPrimitive (val addrBus, val dataBus, val writeEn)
  let output' = register zero output

  -- Methods
  let load a = do
        addrBus <== a

  let store a d = do
        addrBus <== a
        dataBus <== d
        writeEn <== 1

  -- Return interface
  return (RAM load store output output' (val writeEn))

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
  let ramPrimitive = case init of
        Nothing  -> ramTrueDual
        Just str -> ramTrueDualInit str
 
  -- RAM output
  let (outA, outB) = ramPrimitive (val addrBusA, val dataBusA, val writeEnA)
                                  (val addrBusB, val dataBusB, val writeEnB)

  let outA' = register zero outA
  let outB' = register zero outB

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
  return (RAM loadA storeA outA outA' (val writeEnA),
          RAM loadB storeB outB outB' (val writeEnB))

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

  let outMethod' = register zero outMethod

  return (RAM loadMethod storeMethod outMethod outMethod' (writeEn ram))

makeDualRAMPassthroughInit :: (Bits a, Bits d) => String -> RTL (RAM a d)
makeDualRAMPassthroughInit init = makeDualRAMPassthroughCore (Just init)

makeDualRAMPassthrough :: (Bits a, Bits d) => RTL (RAM a d)
makeDualRAMPassthrough = makeDualRAMPassthroughCore Nothing
