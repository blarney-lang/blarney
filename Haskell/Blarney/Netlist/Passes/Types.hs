{-|
Module      : Blarney.Netlist.Passes.Types
Description : Types used for netlist passes
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module defines the notions of mutable netlists 'MNetlist's and passes
'MNetlistPass'es which can be used to write 'Netlist' optimisation passes.

-}

module Blarney.Netlist.Passes.Types (
-- * Mutable netlist types
  MNetlist
, MNetlistRef
, MNetlistPass
) where

import Data.Maybe
import Data.STRef
import Data.Array.ST
import Control.Monad.ST

import Blarney.Core.Prim
import Blarney.Core.NetHelpers

-- | A helper type for mutable 'Netlist'
type MNetlist s = STArray s InstId (Maybe Net)

-- | A reference to an 'MNetlist'
type MNetlistRef s = STRef s (MNetlist s)

-- | A pass over an 'MNetlist'
type MNetlistPass s a = MNetlistRef s -> ST s a
