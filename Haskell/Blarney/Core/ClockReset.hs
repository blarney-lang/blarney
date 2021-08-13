{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Blarney.Core.ClockReset
Description : Types for clocks and resets
Copyright   : (c) Matthew Naylor, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.ClockReset where

-- Standard imports
import Prelude
import GHC.Generics

-- Blarney imports
import Blarney.Core.Bit

data Clock = Clock (Bit 1) deriving Generic

data Reset = Reset (Bit 1) deriving Generic
