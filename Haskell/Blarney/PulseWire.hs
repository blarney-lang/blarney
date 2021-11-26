{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Blarney.PulseWire
Description : A PulseWire hardware module
Copyright   : (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

A PulseWire hardware module
-}
module Blarney.PulseWire
  ( -- * PulseWire
    PulseWire(..), makePulseWire
  ) where

-- Blarney imports
import Blarney

-- | 'PulseWire' type
data PulseWire =
  PulseWire {
    pulse :: Action ()
    -- ^ Sends a pulse on the wire
  , val :: Bit 1
    -- ^ Checks whether a pulse was sent this cycle
  } deriving (Generic, Interface)

-- | 'Val' instance for 'PulseWire', returning whether the wire was pulsed
instance Val PulseWire (Bit 1) where
  val w = w.val

-- | Single-bit wire that emits 0 unless pulsed
makePulseWire :: Module PulseWire
makePulseWire = do
  w :: Wire (Bit 0) <- makeWire dontCare
  return
    PulseWire {
      pulse = do w <== dontCare
    , val = active w
    }
