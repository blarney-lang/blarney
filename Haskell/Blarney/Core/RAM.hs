{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

{-|
Module      : Blarney.Core.RAM
Description : Library of various block RAM components
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.RAM (
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
, makeDualRAMForward  -- Forwarding dual-port block RAM
, makeDualRAMForwardInit -- Initialised forwarding dual-port block RAM
) where

-- Standard imports
import Prelude

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Module
import Blarney.Core.Prelude

-- RAM primitive (for internal use only)
-- (Read during write: reads "dont care")
ramPrim :: Int -> Maybe String -> (Bit a, Bit d, Bit 1) -> Bit d
ramPrim dataWidth init (a, d, en) =
  FromBV $ ramBV dataWidth init (toBV a, toBV d, toBV en)

-- True dual-port RAM primitive (for internal use only)
-- (Read during write: reads "dont care")
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
-- (Read during write: reads "dont care")
ram :: (Bits a, Bits d) => (a, d, Bit 1) -> d
ram (a, d, en) = 
  unpack (ramPrim (sizeOf d) Nothing (pack a, pack d, en))

-- |Initilaised block RAM (contents taken from hex file).
-- (Read during write: reads "dont care")
ramInit :: (Bits a, Bits d) => String -> (a, d, Bit 1) -> d
ramInit init (a, d, en) =
  unpack (ramPrim (sizeOf d) (Just init) (pack a, pack d, en))

-- | Uninitialised true dual-port block RAM.
-- (Read during write: reads "dont care")
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
-- (Read during write: reads "dont care")
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

  -- Return interface
  return RAM {
    load    = (addrBus <==)
  , store   = \a d -> do
                addrBus <== a
                dataBus <== d
                writeEn <== 1
  , out     = ramPrimitive (val addrBus, val dataBus, val writeEn)
  , writeEn = val writeEn
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

  -- Return interface
  return (RAM {
              load    = (addrBusA <==)
            , store   = \a d -> do addrBusA <== a
                                   dataBusA <== d
                                   writeEnA <== 1
            , out     = outA
            , writeEn = val writeEnA
            },
          RAM {
              load    = (addrBusB <==)
            , store   = \a d -> do addrBusB <== a
                                   dataBusB <== d
                                   writeEnB <== 1
            , out     = outB
            , writeEn = val writeEnB
            })

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

  -- Return interface
  return RAM {
    load    = load portA
  , store   = store portB
  , out     = out portA
  , writeEn = writeEn portB
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
  , writeEn = writeEn ram
  }
