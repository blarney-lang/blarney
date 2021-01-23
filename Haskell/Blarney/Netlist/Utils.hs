{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Netlist.Utils
Description : Blarney utils for interaction with 'Netlist's
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module provides means to work with Blarney 'Netlist's. Mainly, it defines
the notions of mutable netlists 'MNetlist's and passes 'MNetlistPass'es which
can be used to write 'Netlist' optimisation passes.

-}

module Blarney.Netlist.Utils (
-- * Types to work with mutable 'Netlist's
  MNetlist
, MNetlistPass
-- * Queries on individual 'Net's
, netIsRoot
, netDontKill
, readNet
-- * Simple 'Net' reference counting pass
, NetCounts
, countNetRef
-- * Exported relevant 'Blarney.Core' modules
, module Blarney.Core.Net
, module Blarney.Core.Prim
-- * Relevant, not already defined monadic operations
, untilM
, untilM_
) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Blarney.Core.Net
import Blarney.Core.Prim

-- | A helper type for mutable 'Netlist'
type MNetlist s = STArray s InstId (Maybe Net)

-- | A type for running a pass over an 'MNetlist'
type MNetlistPass s a = MNetlist s -> ST s a

-- | A helper function to tell if a 'Net' is a netlist root
netIsRoot :: Net -> Bool
netIsRoot Net{netPrim=prim} = primIsRoot prim

-- | A helper function to tell if a 'Net' should not be optimised away
netDontKill :: Net -> Bool
netDontKill Net{netPrim=prim} = primDontKill prim

-- | A helper function to read a 'Net' from a 'MNetlist'
readNet :: InstId -> MNetlistPass s Net
readNet instId mnl = fromMaybe (error "encountered InstId with no matching Net")
                               <$> (readArray mnl instId)

-- | A helper type for 'Net' reference counting
type NetCounts s = STUArray s InstId Int

-- | A 'Net' reference counting helper function
countNetRef :: MNetlistPass s (NetCounts s)
countNetRef mnl = do
  bounds <- getBounds mnl
  refCounts <- newArray bounds 0
  es <- getElems mnl
  -- count references for each Net
  let innerCount (InputWire (instId, _)) = do
        cnt <- readArray refCounts instId
        writeArray refCounts instId (cnt + 1)
      innerCount (InputTree _ inpts) = mapM_ innerCount inpts
  forM_ [e | Just e <- es] $ \net -> mapM_ innerCount (netInputs net)
  -- return reference counts
  return refCounts

-- | Repeat computation until a predicate holds
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM pred act =
  act >>= \x -> if pred x then return x else untilM pred act

-- | Same as 'untilM' but discard the final result
untilM_ :: Monad m => (a -> Bool) -> m a -> m ()
untilM_ pred act = do untilM pred act
                      return ()
