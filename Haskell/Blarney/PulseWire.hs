{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
    PulseWire(..), Pulse(..), makePulseWire
  ) where

-- Blarney imports
import Blarney

-- | 'PulseWire' type
data PulseWire =
  PulseWire { pulseWireSend   :: Action () -- ^ Sends a pulse
            , pulseWirePulsed :: Bit 1     -- ^ Checks whether a pulse was sent
                                           --   this cycle
            }

-- | 'Pulse' class
class Pulse v where
  -- | Sends a pulse
  pulse :: v -> Action ()

-- | 'Pulse' instance for 'PulseWire'
instance Pulse PulseWire where
  pulse = pulseWireSend

-- | 'Val' instance for 'PulseWire', returning whether the wire was pulsed
instance Val PulseWire (Bit 1) where
  val = pulseWirePulsed

-- | Constructs a 'PulseWire' 'Module'
makePulseWire :: Module PulseWire
makePulseWire = do
  w :: Wire (Bit 0) <- makeWire dontCare
  return PulseWire {
    pulseWireSend   = w <== dontCare
  , pulseWirePulsed = active w
  }
