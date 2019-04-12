{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blarney.PulseWire
  ( -- * PulseWire
    Pulse(..), PulseWire(..), makePulseWire
  ) where

-- Blarney imports
import Blarney

-- |Bit 1 pulse
class Pulse v where
  pulse :: v -> Action ()

-- |Pulse Wire
data PulseWire =
  PulseWire {
    -- |Send a pulse
    pulseWireSend :: Action ()
    -- |Is pulse present this cycle ?
  , pulseWirePulsed :: Bit 1
  }
instance Pulse PulseWire where
  pulse = pulseWireSend
instance Val PulseWire (Bit 1) where
  val = pulseWirePulsed

-- |Create a PulseWire
makePulseWire :: Module PulseWire
makePulseWire = do
  w :: Wire (Bit 0) <- makeWire dontCare
  return PulseWire {
    pulseWireSend   = w <== dontCare
  , pulseWirePulsed = active w
  }
