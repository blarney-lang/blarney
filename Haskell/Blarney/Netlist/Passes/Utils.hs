{-# LANGUAGE NoRebindableSyntax #-}

{- |
Module      : Blarney.Netlist.Passes.Utils
Description : Utility functions for netlist passes
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module provides helper functions to write netlist passes.

-}

module Blarney.Netlist.Passes.Utils (
-- * Queries on individual 'Net's
  netIsRoot
, netDontKill
, readNet
-- * Basic 'MNetlistPass'es
, NetCounts
, countNetRef
-- * Exported 'Blarney' modules
, module Blarney.Core.Net
, module Blarney.Core.Prim
) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST

import Blarney.Core.Net
import Blarney.Core.Prim
import Blarney.Netlist.Passes.Types

-- | A helper function to tell if a 'Net' is a netlist root
netIsRoot :: Net -> Bool
netIsRoot Net{netPrim=prim} = primIsRoot prim

-- | A helper function to tell if a 'Net' should not be optimised away
netDontKill :: Net -> Bool
netDontKill Net{netPrim=prim} = primDontKill prim

-- | A helper function to read a 'Net' from a 'MNetlist'
readNet :: InstId -> MNetlistPass s Net
readNet instId mnlRef = do
  mnl <- readSTRef mnlRef
  fromMaybe (error "encountered InstId with no matching Net")
            <$> readArray mnl instId

-- | A helper type for 'Net' reference counting
type NetCounts s = STUArray s InstId Int

-- | A 'Net' reference counting helper function
countNetRef :: MNetlistPass s (NetCounts s)
countNetRef mnlRef = do
  mnl <- readSTRef mnlRef
  bounds <- getBounds mnl
  refCounts <- newArray bounds 0
  es <- getElems mnl
  -- count references for each Net
  let innerCount (InputWire (instId, _)) = do
        cnt <- readArray refCounts instId
        writeArray refCounts instId $! (cnt + 1)
      innerCount (InputTree _ inpts) = mapM_ innerCount inpts
  forM_ [e | Just e <- es] $ \net -> mapM_ innerCount (netInputs net)
  -- return reference counts
  return refCounts
