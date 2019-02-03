{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

{-|
Module      : Blarney.RAM
Description : Library of various block RAM components
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.RAM 
  (
    -- * Block RAM primitives
    ram                 -- Block RAM primitive
  , ramInit             -- Initialised block RAM primitive
  , ramTrueDual         -- True dual-port block RAM primitive
  , ramTrueDualInit     -- Initialised true dual-port block RAM primitive

    -- * RTL block RAM interface
  , RAM(..)             -- Block RAM interface
  , makeRAM             -- Block RAM
  , makeRAMInit         -- Initialised block RAM
  , makeTrueDualRAM     -- True dual-port block RAM
  , makeTrueDualRAMInit -- Initialised true dual-port block RAM
  , makeDualRAM         -- Dual-port block RAM
  , makeDualRAMInit     -- Initialised dual-port block RAM
  , makeDualRAMPassthrough     -- Pass-through dual-port block RAM
  , makeDualRAMPassthroughInit -- Initialised pass-through dual-port block RAM 
  ) where

-- Standard imports
import Prelude

-- Blarney imports
import Blarney.BV
import Blarney.Bit
import Blarney.Bits
import Blarney.Module
import Blarney.Prelude

-- RAM primitive (for internal use only)
-- (Reads new data on write)
ramPrim :: Int -> Maybe String -> (Bit a, Bit d, Bit 1) -> Bit d
ramPrim dataWidth init (a, d, en) =
  FromBV $ ramBV dataWidth init (toBV a, toBV d, toBV en)

-- True dual-port RAM primitive (for internal use only)
-- (Reads new data on write)
-- (When read-address == write-address on different ports, read old data)
ramTrueDualPrim :: Int -> Maybe String
                -> (Bit a, Bit d, Bit 1)
                -> (Bit a, Bit d, Bit 1)
                -> (Bit d, Bit d)
ramTrueDualPrim dataWidth init (a0, d0, en0)
                               (a1, d1, en1) =
   (FromBV o0, FromBV o1)
  where
    (o0, o1) = dualRamBV dataWidth init
                 (toBV a0, toBV d0, toBV en0)
                 (toBV a1, toBV d1, toBV en1)

-- |Uninitialised block RAM.
-- Reads new data on write.
ram :: (Bits a, Bits d) => (a, d, Bit 1) -> d
ram (a, d, en) = 
  unpack (ramPrim (sizeOf d) Nothing (pack a, pack d, en))

-- |Initilaised block RAM (contents taken from hex file).
-- Reads new data on write.
ramInit :: (Bits a, Bits d) => String -> (a, d, Bit 1) -> d
ramInit init (a, d, en) =
  unpack (ramPrim (sizeOf d) (Just init) (pack a, pack d, en))

-- | Uninitialised true dual-port block RAM.
-- Reads new data on write.
-- When read-address == write-address on different ports, reads old data.
ramTrueDual :: (Bits a, Bits d) =>
               (a, d, Bit 1)
            -> (a, d, Bit 1)
            -> (d, d)
ramTrueDual (a0, d0, en0)
            (a1, d1, en1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) Nothing
                 (pack a0, pack d0, en0)
                 (pack a1, pack d1, en1)

-- Initilaised true dual-port block RAM.
-- Reads new data on write.
-- When read-address == write-address on different ports, reads old data.
ramTrueDualInit :: (Bits a, Bits d) =>
                   String
                -> (a, d, Bit 1)
                -> (a, d, Bit 1)
                -> (d, d)
ramTrueDualInit init (a0, d0, en0)
                     (a1, d1, en1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) (Just init)
                 (pack a0, pack d0, en0)
                 (pack a1, pack d1, en1)

-- |RAM interface
data RAM a d =
  RAM {
    load    :: a -> Action ()       -- ^ Issue load request
  , store   :: a -> d -> Action ()  -- ^ Issue store request
  , out     :: d                    -- ^ Data out
  , out'    :: d                    -- ^ Registered data out
  , writeEn :: Bit 1                -- ^ Is there a store currently happening?
  }

-- | Create uninitialised block RAM
makeRAM :: (Bits a, Bits d) => Module (RAM a d)
makeRAM = makeRAMCore Nothing

-- | Create block RAM with initial contents from hex file
makeRAMInit :: (Bits a, Bits d) => String -> Module (RAM a d)
makeRAMInit init = makeRAMCore (Just init)

-- Core RAM module
makeRAMCore :: (Bits a, Bits d) => Maybe String -> Module (RAM a d)
makeRAMCore init = do
  -- Address bus and data bus and write-enable
  addrBus :: Wire a <- makeWireU
  dataBus :: Wire d <- makeWireU
  writeEn :: Wire (Bit 1) <- makeWire 0

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ram
        Just str -> ramInit str

  -- RAM output
  let output = ramPrimitive (val addrBus, val dataBus, val writeEn)
  let output' = delay zero output

  -- Methods
  let load a = do
        addrBus <== a

  let store a d = do
        addrBus <== a
        dataBus <== d
        writeEn <== 1

  -- Return interface
  return (RAM load store output output' (val writeEn))

-- |Create true dual-port block RAM.
-- When read-address == write-address on different ports, read old data.
makeTrueDualRAM :: (Bits a, Bits d) => Module (RAM a d, RAM a d)
makeTrueDualRAM = makeTrueDualRAMCore Nothing

-- |Create true dual-port block RAM with initial contents from hex file.
-- When read-address == write-address on different ports, read old data.
makeTrueDualRAMInit :: (Bits a, Bits d) => String -> Module (RAM a d, RAM a d)
makeTrueDualRAMInit init = makeTrueDualRAMCore (Just init)

-- True dual-port core RAM module
makeTrueDualRAMCore :: (Bits a, Bits d) =>
                       Maybe String -> Module (RAM a d, RAM a d)
makeTrueDualRAMCore init = do
  -- Address bus and data bus and write-enable
  addrBusA :: Wire a <- makeWireU
  dataBusA :: Wire d <- makeWireU
  writeEnA :: Wire (Bit 1) <- makeWire 0

  addrBusB :: Wire a <- makeWireU
  dataBusB :: Wire d <- makeWireU
  writeEnB :: Wire (Bit 1) <- makeWire 0

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramTrueDual
        Just str -> ramTrueDualInit str
 
  -- RAM output
  let (outA, outB) = ramPrimitive (val addrBusA, val dataBusA, val writeEnA)
                                  (val addrBusB, val dataBusB, val writeEnB)

  let outA' = delay zero outA
  let outB' = delay zero outB

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

-- |Create uninitialised dual-port RAM.
-- One port used for reading and the other for writing.
makeDualRAM :: (Bits a, Bits d) => Module (RAM a d)
makeDualRAM = makeDualRAMCore Nothing

-- |Create dual-port RAM with initial contents from hex file.
-- One port used for reading and the other for writing.
makeDualRAMInit :: (Bits a, Bits d) => String -> Module (RAM a d)
makeDualRAMInit init = makeDualRAMCore (Just init)

-- Dual port RAM module.
-- One port used for reading and the other for writing.
makeDualRAMCore :: (Bits a, Bits d) => Maybe String -> Module (RAM a d)
makeDualRAMCore init = do
  -- Create true dual port RAM
  (portA :: RAM a d, portB :: RAM a d) <- makeTrueDualRAMCore init

  -- Methods
  let loadA a = load portA a

  let storeB a d = store portB a d

  -- Return interface
  return (RAM loadA storeB (out portA) (out' portA) (writeEn portB))

-- | Dual-port passthrough block RAM with initial contents from hex file.
-- Read and write to same address yields new data.
makeDualRAMPassthroughInit :: (Bits a, Bits d) => String -> Module (RAM a d)
makeDualRAMPassthroughInit init = makeDualRAMPassthroughCore (Just init)

-- | Uninitialised dual-port passthrough block RAM.
-- Read and write to same address yields new data.
makeDualRAMPassthrough :: (Bits a, Bits d) => Module (RAM a d)
makeDualRAMPassthrough = makeDualRAMPassthroughCore Nothing

-- Dual port RAM module with pass-through.
-- Read and write to same address yields new data.
makeDualRAMPassthroughCore :: (Bits a, Bits d) =>
                                 Maybe String -> Module (RAM a d)
makeDualRAMPassthroughCore init = do
  -- Create dual port RAM
  ram :: RAM a d <- makeDualRAMCore init

  -- Details of lastest load and store
  la :: Wire a <- makeWireU
  sa :: Wire a <- makeWireU
  sd :: Wire d <- makeWireU

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
           (val' la === val' sa)) ?
              (val' sd, out ram)

  let outMethod' = delay zero outMethod

  return (RAM loadMethod storeMethod outMethod outMethod' (writeEn ram))
