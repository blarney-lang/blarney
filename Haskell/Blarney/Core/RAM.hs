{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module      : Blarney.Core.RAM
Description : Library of various block RAM components
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.RAM (
  -- * Block RAM primitives
  ram                 -- Block RAM primitive
, ramInit             -- Initialised block RAM primitive
, ramDual             -- Simple dual-port block RAM primitive
, ramDualInit         -- Initialised simple dual-port block RAM primitive
, ramTrueDual         -- True dual-port block RAM primitive
, ramTrueDualInit     -- Initialised true dual-port block RAM primitive

  -- * Block RAM primitives with byte enables
, ramBE               -- Block RAM primitive
, ramInitBE           -- Initialised block RAM primitive
, ramDualBE           -- Simple dual-port block RAM primitive
, ramDualInitBE       -- Initialised simple dual-port block RAM primitive
, ramTrueDualBE       -- True dual-port block RAM primitive
, ramTrueDualInitBE   -- Initialised true dual-port block RAM primitive

  -- * RTL block RAM interface
, RAM(..)             -- Block RAM interface
, makeRAM             -- Block RAM
, makeRAMInit         -- Initialised block RAM
, makeRAMCore         -- Optionally initialised block RAM
, makeTrueDualRAM     -- True dual-port block RAM
, makeTrueDualRAMInit -- Initialised true dual-port block RAM
, makeTrueDualRAMCore -- Optionally initialised true dual-port block RAM
, makeDualRAM         -- Dual-port block RAM
, makeDualRAMInit     -- Initialised dual-port block RAM
, makeDualRAMCore     -- Optionally initialised dual-port block RAM
, makeDualRAMForward  -- Forwarding dual-port block RAM
, makeDualRAMForwardInit -- Initialised forwarding dual-port block RAM
, nullRAM             -- Make a RAM interface with no RAM backing it

  -- * RTL block RAM interface with byte enables
, RAMBE(..)              -- Block RAM interface
, makeRAMBE              -- Block RAM
, makeRAMInitBE          -- Intialised block RAM
, makeRAMBECore          -- Optionally intialised block RAM
, makeTrueDualRAMBE      -- True dual-port block RAM
, makeTrueDualRAMInitBE  -- Initialised true dual-port block RAM
, makeTrueDualRAMBECore  -- Optionally initialised true dual-port block RAM
, makeDualRAMBE          -- Dual-port block RAM
, makeDualRAMInitBE      -- Initilaised dual-port block RAM
, makeDualRAMBECore      -- Initilaised dual-port block RAM
, nullRAMBE              -- Make a RAMBE interface with no RAM backing it
) where

-- Standard imports
import Prelude

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Module
import Blarney.Core.Prelude

-- GHC imports
import GHC.TypeNats

-- |RAM primitive (for internal use only)
-- (Read during write: reads "dont care")
ramPrim :: Int -> Maybe String -> (Bit a, Bit d, Bit 1, Bit 1) -> Bit d
ramPrim dataWidth init (a, d, we, re) =
  FromBV $ ramBV dataWidth init (toBV a, toBV d, toBV we, toBV re, Nothing)

-- |Simple dual-port RAM primitive (for internal use only)
-- (Read during write: reads "dont care")
ramDualPrim :: Int -> Maybe String ->
  (Bit a, Bit a, Bit d, Bit 1, Bit 1) -> Bit d
ramDualPrim dataWidth init (ra, wa, d, we, re) =
  FromBV $ dualRamBV dataWidth init
    (toBV ra, toBV wa, toBV d, toBV we, toBV re, Nothing)

-- |True dual-port RAM primitive (for internal use only)
-- (Read during write: reads "dont care")
ramTrueDualPrim :: Int -> Maybe String
                -> (Bit a, Bit d, Bit 1, Bit 1)
                -> (Bit a, Bit d, Bit 1, Bit 1)
                -> (Bit d, Bit d)
ramTrueDualPrim dataWidth init (a0, d0, we0, re0)
                               (a1, d1, we1, re1) =
   (FromBV o0, FromBV o1)
  where
    (o0, o1) = trueDualRamBV dataWidth init
                 (toBV a0, toBV d0, toBV we0, toBV re0, Nothing)
                 (toBV a1, toBV d1, toBV we1, toBV re1, Nothing)

-- |Uninitialised block RAM.
-- (Read during write: reads "dont care")
ram :: (Bits a, Bits d) => (a, d, Bit 1, Bit 1) -> d
ram (a, d, we, re) =
  unpack (ramPrim (sizeOf d) Nothing (pack a, pack d, we, re))

-- |Initilaised block RAM (contents taken from hex file).
-- (Read during write: reads "dont care")
ramInit :: (Bits a, Bits d) => String -> (a, d, Bit 1, Bit 1) -> d
ramInit init (a, d, we, re) =
  unpack (ramPrim (sizeOf d) (Just init) (pack a, pack d, we, re))

-- |Uninitialised simple dual-port block RAM.
-- (Read during write: reads "dont care")
ramDual :: (Bits a, Bits d) => (a, a, d, Bit 1, Bit 1) -> d
ramDual (ra, wa, d, we, re) =
  unpack (ramDualPrim (sizeOf d) Nothing (pack ra, pack wa, pack d, we, re))

-- |Initialised simple dual-port block RAM.
-- (Read during write: reads "dont care")
ramDualInit :: (Bits a, Bits d) => String -> (a, a, d, Bit 1, Bit 1) -> d
ramDualInit init (ra, wa, d, we, re) =
  unpack (ramDualPrim (sizeOf d) (Just init) (pack ra, pack wa, pack d, we, re))

-- | Uninitialised true dual-port block RAM.
-- (Read during write: reads "dont care")
ramTrueDual :: (Bits a, Bits d) =>
               (a, d, Bit 1, Bit 1)
            -> (a, d, Bit 1, Bit 1)
            -> (d, d)
ramTrueDual (a0, d0, we0, re0)
            (a1, d1, we1, re1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) Nothing
                 (pack a0, pack d0, we0, re0)
                 (pack a1, pack d1, we1, re1)

-- Initilaised true dual-port block RAM.
-- (Read during write: reads "dont care")
ramTrueDualInit :: (Bits a, Bits d) =>
                   String
                -> (a, d, Bit 1, Bit 1)
                -> (a, d, Bit 1, Bit 1)
                -> (d, d)
ramTrueDualInit init (a0, d0, we0, re0)
                     (a1, d1, we1, re1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) (Just init)
                 (pack a0, pack d0, we0, re0)
                 (pack a1, pack d1, we1, re1)

-- |RAM primitive with byte enables (for internal use only)
-- (Read during write: reads "dont care")
ramPrimBE :: Int -> Maybe String
          -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramPrimBE dataWidth init (a, d, we, re, be) =
  FromBV $ ramBV dataWidth init
    (toBV a, toBV d, toBV we, toBV re, Just (toBV be))

-- |Simple dual-port RAM primitive with byte enables (for internal use only)
-- (Read during write: reads "dont care")
ramDualPrimBE :: Int -> Maybe String
              -> (Bit a, Bit a, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramDualPrimBE dataWidth init (ra, wa, d, we, re, be) =
  FromBV $ dualRamBV dataWidth init
    (toBV ra, toBV wa, toBV d, toBV we, toBV re, Just (toBV be))

-- |True dual-port RAM primitive with byte enables (for internal use only)
-- (Read during write: reads "dont care")
ramTrueDualPrimBE :: Int -> Maybe String
                  -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
                  -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
                  -> (Bit (8*be), Bit (8*be))
ramTrueDualPrimBE dataWidth init (a0, d0, we0, re0, be0)
                                 (a1, d1, we1, re1, be1) =
    (FromBV o0, FromBV o1)
  where
    (o0, o1) = trueDualRamBV dataWidth init
                 (toBV a0, toBV d0, toBV we0, toBV re0, Just (toBV be0))
                 (toBV a1, toBV d1, toBV we1, toBV re1, Just (toBV be1))

-- |Uninitialised block RAM.
-- (Read during write: reads "dont care")
ramBE :: KnownNat be => (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramBE (a, d, we, re, be) =
  ramPrimBE (8 * widthOf be) Nothing (a, d, we, re, be)

-- |Initilaised block RAM (contents taken from hex file).
-- (Read during write: reads "dont care")
ramInitBE :: KnownNat be =>
  String -> (Bit aw, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramInitBE init (a, d, we, re, be) =
  ramPrimBE (8 * widthOf be) (Just init) (a, d, we, re, be)

-- |Uninitialised block RAM.
-- (Read during write: reads "dont care")
ramDualBE :: KnownNat be =>
  (Bit aw, Bit aw, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramDualBE (ra, wa, d, we, re, be) =
  ramDualPrimBE (8 * widthOf be) Nothing (ra, wa, d, we, re, be)

-- |Initialised block RAM (contents taken from hex file).
-- (Read during write: reads "dont care")
ramDualInitBE :: KnownNat be =>
  String -> (Bit aw, Bit aw, Bit (8*be), Bit 1, Bit 1, Bit be) -> Bit (8*be)
ramDualInitBE init (ra, wa, d, we, re, be) =
  ramDualPrimBE (8 * widthOf be) (Just init) (ra, wa, d, we, re, be)

-- | Uninitialised true dual-port block RAM.
-- (Read during write: reads "dont care")
ramTrueDualBE :: KnownNat be =>
     (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
  -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
  -> (Bit (8*be), Bit (8*be))
ramTrueDualBE (a0, d0, we0, re0, be0)
              (a1, d1, we1, re1, be1) = (o0, o1)
  where
    (o0, o1) = ramTrueDualPrimBE (8 * widthOf be0) Nothing
                 (a0, d0, we0, re0, be0)
                 (a1, d1, we1, re1, be1)

-- Initilaised true dual-port block RAM.
-- (Read during write: reads "dont care")
ramTrueDualInitBE :: KnownNat be =>
     String
  -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
  -> (Bit a, Bit (8*be), Bit 1, Bit 1, Bit be)
  -> (Bit (8*be), Bit (8*be))
ramTrueDualInitBE init (a0, d0, we0, re0, be0)
                       (a1, d1, we1, re1, be1) = (o0, o1)
  where
    (o0, o1) = ramTrueDualPrimBE (8 * widthOf be0) (Just init)
                 (a0, d0, we0, re0, be0)
                 (a1, d1, we1, re1, be1)

-- |RAM interface
data RAM a d =
  RAM {
    load :: a -> Action ()        -- ^ Issue load request
  , store :: a -> d -> Action ()  -- ^ Issue store request
  , out :: d                      -- ^ Data out
  , storeActive :: Bit 1          -- ^ Is there a store currently happening?
  , preserveOut :: Action ()      -- ^ Preserve current output on next cycle
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
  readEn  :: Wire (Bit 1) <- makeWire 1

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ram
        Just str -> ramInit str

  -- Return interface
  return RAM {
    load    = (addrBus <==)
  , store   = \a d -> do
                addrBus <== a
                dataBus <== d
                writeEn <== 1
  , out     = ramPrimitive (val addrBus, val dataBus, val writeEn, val readEn)
  , storeActive = val writeEn
  , preserveOut = readEn <== 0
  }

-- |Create true dual-port block RAM.
-- (Read during write: reads "dont care")
makeTrueDualRAM :: (Bits a, Bits d) => Module (RAM a d, RAM a d)
makeTrueDualRAM = makeTrueDualRAMCore Nothing

-- |Create true dual-port block RAM with initial contents from hex file.
-- (Read during write: reads "dont care")
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
  readEnA  :: Wire (Bit 1) <- makeWire 1

  addrBusB :: Wire a <- makeWireU
  dataBusB :: Wire d <- makeWireU
  writeEnB :: Wire (Bit 1) <- makeWire 0
  readEnB  :: Wire (Bit 1) <- makeWire 1

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramTrueDual
        Just str -> ramTrueDualInit str

  -- RAM output
  let (outA, outB) = ramPrimitive
        (val addrBusA, val dataBusA, val writeEnA, val readEnA)
        (val addrBusB, val dataBusB, val writeEnB, val readEnB)

  -- Return interface
  return (RAM {
              load    = (addrBusA <==)
            , store   = \a d -> do addrBusA <== a
                                   dataBusA <== d
                                   writeEnA <== 1
            , out     = outA
            , storeActive = val writeEnA
            , preserveOut = readEnA <== 0
            },
          RAM {
              load    = (addrBusB <==)
            , store   = \a d -> do addrBusB <== a
                                   dataBusB <== d
                                   writeEnB <== 1
            , out     = outB
            , storeActive = val writeEnB
            , preserveOut = readEnB <== 0
            })

-- |Create uninitialised dual-port RAM.
-- One port used for reading and the other for writing.
makeDualRAM :: (Bits a, Bits d) => Module (RAM a d)
makeDualRAM = makeDualRAMCore Nothing

-- |Create dual-port RAM with initial contents from hex file.
-- One port used for reading and the other for writing.
makeDualRAMInit :: (Bits a, Bits d) => String -> Module (RAM a d)
makeDualRAMInit init = makeDualRAMCore (Just init)

-- Simple dual port RAM module.
-- One port used for reading and the other for writing.
makeDualRAMCore :: (Bits a, Bits d) => Maybe String -> Module (RAM a d)
makeDualRAMCore init = do
  -- Address busses and data bus and write-enable
  rdAddrBus :: Wire a <- makeWireU
  wrAddrBus :: Wire a <- makeWireU
  dataBus   :: Wire d <- makeWireU
  writeEn   :: Wire (Bit 1) <- makeWire 0
  readEn    :: Wire (Bit 1) <- makeWire 1

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramDual
        Just str -> ramDualInit str

  -- Return interface
  return RAM {
    load    = (rdAddrBus <==)
  , store   = \a d -> do
                wrAddrBus <== a
                dataBus <== d
                writeEn <== 1
  , out     = ramPrimitive (val rdAddrBus, val wrAddrBus,
                            val dataBus, val writeEn, val readEn)
  , storeActive = val writeEn
  , preserveOut = readEn <== 0
  }

-- | Dual-port forwarding block RAM with initial contents from hex file.
-- Read and write to same address yields new data.
makeDualRAMForwardInit :: (Bits a, Bits d) => Int -> String -> Module (RAM a d)
makeDualRAMForwardInit n init = makeDualRAMForwardCore n (Just init)

-- | Uninitialised dual-port forwarding block RAM.
-- Read and write to same address yields new data.
makeDualRAMForward :: (Bits a, Bits d) => Int -> Module (RAM a d)
makeDualRAMForward n = makeDualRAMForwardCore n Nothing

-- Dual port RAM module with forwarding.
-- Read and write to same address yields new data.
makeDualRAMForwardCore :: (Bits a, Bits d) =>
                                 Int -> Maybe String -> Module (RAM a d)
makeDualRAMForwardCore n init
  | n < 0 = error "makeDualRAMForwardCore: n must be greater or equal to 0"
  | otherwise = do
  -- Create dual port RAM
  ram :: RAM a d <- makeDualRAMCore init

  -- Details of lastest load and store
  la :: Wire a <- makeWireU
  sa :: Wire a <- makeWireU
  sd :: Wire d <- makeWireU

  let get_rdata (_,_,x,_,_,_) = x
  let cmpStep (ractive, raddr, rdata, wactive, waddr, wdata) =
       ( ractive
       , raddr
       , (ractive .&. wactive .&. (raddr === waddr)) ? (wdata, rdata)
       , wactive
       , waddr
       , wdata)
  let latchStep (ractive, raddr, rdata, wactive, waddr, wdata) =
       ( delay 0 ractive
       , buffer raddr
       , rdata
       , wactive
       , waddr
       , wdata)
  let firstStage = cmpStep ( delay 0 (active la)
                           , buffer (val la)
                           , out ram
                           , delay 0 (active sa)
                           , buffer (val sa)
                           , buffer (val sd))
  let allStages = iterate (cmpStep `o` latchStep) ( active la
                                                  , val la
                                                  , get_rdata firstStage
                                                  , active sa
                                                  , val sa
                                                  , val sd)

  return RAM {
    load    = \a -> do load ram a
                       la <== a
  , store   = \a d -> do store ram a d
                         sa <== a
                         sd <== d
  , out     = get_rdata $ allStages !! n
  , storeActive = storeActive ram
  , preserveOut = preserveOut ram
  }

-- |RAM (with byte enables) interface (data width is in bytes)
data RAMBE aw dw =
  RAMBE {
    -- | Issue load request
    loadBE :: Bit aw -> Action ()
    -- | Issue store request, with byte enables
  , storeBE :: Bit aw -> Bit dw -> Bit (8*dw) -> Action ()
    -- | Data out
  , outBE :: Bit (8*dw)
    -- | Is there a store currently happening?
  , storeActiveBE :: Bit 1
    -- | Preserve current output on next cycle
  , preserveOutBE :: Action ()
  }

-- | Create uninitialised block RAM with byte enables
makeRAMBE :: _ => Module (RAMBE aw dw)
makeRAMBE = makeRAMBECore Nothing

-- | Create block RAM with byte enables and initial contents from hex file
makeRAMInitBE :: _ => String -> Module (RAMBE aw dw)
makeRAMInitBE init = makeRAMBECore (Just init)

-- Core RAM module with byte enables
makeRAMBECore :: _ => Maybe String -> Module (RAMBE aw dw)
makeRAMBECore init = do
  -- Address bus and data bus and write-enable
  addrBus :: Wire (Bit aw) <- makeWireU
  dataBus :: Wire (Bit (8*dw)) <- makeWireU
  writeEn :: Wire (Bit 1) <- makeWire 0
  readEn  :: Wire (Bit 1) <- makeWire 1
  byteEn  :: Wire (Bit dw) <- makeWire 0

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramBE
        Just str -> ramInitBE str

  -- Return interface
  return RAMBE {
    loadBE = (addrBus <==)
  , storeBE = \a be d -> do
                addrBus <== a
                dataBus <== d
                writeEn <== 1
                byteEn  <== be
  , outBE = ramPrimitive
      (val addrBus, val dataBus, val writeEn, val readEn, val byteEn)
  , storeActiveBE = val writeEn
  , preserveOutBE = readEn <== 0
  }

-- |Create true dual-port block RAM with byte enables
-- (Read during write: reads "dont care")
makeTrueDualRAMBE :: _ => Module (RAMBE aw dw, RAMBE aw dw)
makeTrueDualRAMBE = makeTrueDualRAMBECore Nothing

-- |Create true dual-port block RAM with byte enables and initial contents
-- from hex file (Read during write: reads "dont care")
makeTrueDualRAMInitBE :: _ => String -> Module (RAMBE aw dw, RAMBE aw dw)
makeTrueDualRAMInitBE init = makeTrueDualRAMBECore (Just init)

-- True dual-port core RAM module
makeTrueDualRAMBECore :: _ => Maybe String -> Module (RAMBE aw dw, RAMBE aw dw)
makeTrueDualRAMBECore init = do
  -- Address bus and data bus and write-enable
  addrBusA :: Wire (Bit aw) <- makeWireU
  dataBusA :: Wire (Bit (8*dw)) <- makeWireU
  writeEnA :: Wire (Bit 1) <- makeWire 0
  readEnA  :: Wire (Bit 1) <- makeWire 1
  byteEnA  :: Wire (Bit dw) <- makeWireU

  addrBusB :: Wire (Bit aw) <- makeWireU
  dataBusB :: Wire (Bit (8*dw)) <- makeWireU
  writeEnB :: Wire (Bit 1) <- makeWire 0
  readEnB  :: Wire (Bit 1) <- makeWire 1
  byteEnB  :: Wire (Bit dw) <- makeWireU

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramTrueDualBE
        Just str -> ramTrueDualInitBE str

  -- RAM output
  let (outA, outB) = ramPrimitive
        (val addrBusA, val dataBusA, val writeEnA, val readEnA, val byteEnA)
        (val addrBusB, val dataBusB, val writeEnB, val readEnB, val byteEnB)

  -- Return interface
  return (RAMBE {
              loadBE  = (addrBusA <==)
            , storeBE = \a be d -> do addrBusA <== a
                                      dataBusA <== d
                                      writeEnA <== 1
                                      byteEnA  <== be
            , outBE   = outA
            , storeActiveBE = val writeEnA
            , preserveOutBE = readEnA <== 0
            },
          RAMBE {
              loadBE  = (addrBusB <==)
            , storeBE = \a be d -> do addrBusB <== a
                                      dataBusB <== d
                                      writeEnB <== 1
                                      byteEnB  <== be
            , outBE   = outB
            , storeActiveBE = val writeEnB
            , preserveOutBE = readEnB <== 0
            })

-- |Create uninitialised dual-port RAM with byte enables.
-- One port used for reading and the other for writing.
makeDualRAMBE :: _ => Module (RAMBE aw dw)
makeDualRAMBE = makeDualRAMBECore Nothing

-- |Create dual-port RAM with byte enables and initial contents from hex file.
-- One port used for reading and the other for writing.
makeDualRAMInitBE :: _ => String -> Module (RAMBE aw dw)
makeDualRAMInitBE init = makeDualRAMBECore (Just init)

-- Dual port RAM module.
-- One port used for reading and the other for writing.
makeDualRAMBECore :: _ => Maybe String -> Module (RAMBE aw dw)
makeDualRAMBECore init = do
  -- Address busses and data bus and write-enable
  rdAddrBus :: Wire (Bit aw) <- makeWireU
  wrAddrBus :: Wire (Bit aw) <- makeWireU
  dataBus   :: Wire (Bit (8*dw)) <- makeWireU
  writeEn   :: Wire (Bit 1) <- makeWire 0
  readEn    :: Wire (Bit 1) <- makeWire 1
  byteEn    :: Wire (Bit dw) <- makeWire 0

  -- RAM primitive
  let ramPrimitive = case init of
        Nothing  -> ramDualBE
        Just str -> ramDualInitBE str

  -- Return interface
  return RAMBE {
    loadBE    = (rdAddrBus <==)
  , storeBE   = \a be d -> do
                  wrAddrBus <== a
                  dataBus <== d
                  writeEn <== 1
                  byteEn  <== be
  , outBE     = ramPrimitive (val rdAddrBus, val wrAddrBus, val dataBus,
                              val writeEn, val readEn, val byteEn)
  , storeActiveBE = val writeEn
  , preserveOutBE = readEn <== 0
  }

-- | RAM interface with no backing functionality
nullRAM :: Bits d => RAM a d
nullRAM =
  RAM {
    load = \_ -> return ()
  , store = \_ _ -> return ()
  , out = dontCare
  , storeActive = false
  , preserveOut = return ()
  }

-- | RAMBE interface with no backing functionality
nullRAMBE :: KnownNat (8*dw) => RAMBE aw dw
nullRAMBE =
  RAMBE {
    loadBE = \_ -> return ()
  , storeBE = \_ _ _ -> return ()
  , outBE = dontCare
  , storeActiveBE = false
  , preserveOutBE = return ()
  }
