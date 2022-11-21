{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Blarney.PulseReg
Description : Like PulseWire, but delayed by a cycle
Copyright   : (c) Matthew Naylor, 2022
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

An assignment to a PulseReg is visible only for one cycle, on the
cycle after which the assignment is made.

-}

module Blarney.PulseReg
  ( PulseReg(..)
  , makePulseReg
  ) where

-- Blarney imports
import Blarney

-- | Emits a pulse on the cycle after the pulse method is called
data PulseReg =
  PulseReg {
    pulse :: Action ()
    -- ^ Trigger pulse on next cycle
  , val :: Bit 1
    -- ^ Is the register currently pulsing?
  } deriving (Generic, Interface)

makePulseReg :: Module PulseReg
makePulseReg = do
  -- Implement using a wire that is delayed by one cycle
  w <- makeWire false
  return
    PulseReg {
      pulse = do w <== true
    , val = delay false w.val
    }
