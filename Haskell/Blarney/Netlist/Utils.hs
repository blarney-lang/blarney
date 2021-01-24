{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoRebindableSyntax    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
, MNetlistPassCtxt(..)
, MNetlistPassCtxtRef
-- * Queries on individual 'Net's
, netIsRoot
, netDontKill
, readNet
-- * Basic 'MNetlistPass'es
, NetCounts
, countNetRef
, prune
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
import Data.STRef
import Data.Array.ST

import Blarney.Core.Net
import Blarney.Core.Prim

-- | A helper type for mutable 'Netlist'
type MNetlist s = STArray s InstId (Maybe Net)

-- | A type describing context for 'MNetlistPass'es
data MNetlistPassCtxt s = MNetlistPassCtxt { mnpNetlist :: MNetlist s }

-- | A type describing a reference to an 'MNetlistPassCtxt'
type MNetlistPassCtxtRef s = STRef s (MNetlistPassCtxt s)

-- | A type for running a pass over an 'MNetlist'
type MNetlistPass s a = MNetlistPassCtxtRef s -> ST s a

instance ToNetlist (STRef s (MNetlistPassCtxt s)) (ST s) where
  toNetlist ctxtRef = do
    -- prune the netlist of all 'Nothing' entries
    prune ctxtRef
    -- extract the mutable netlist and freeze it, dropping the Maybe wrapping
    MNetlistPassCtxt {..} <- readSTRef ctxtRef
    x <- mapArray (fromMaybe $ error "Blarney.Netlist.Utils:\
                                     \toNetlist encountered non Just entry\
                                     \after a netlist pruning")
                  mnpNetlist
    freeze x

-- | A helper function to tell if a 'Net' is a netlist root
netIsRoot :: Net -> Bool
netIsRoot Net{netPrim=prim} = primIsRoot prim

-- | A helper function to tell if a 'Net' should not be optimised away
netDontKill :: Net -> Bool
netDontKill Net{netPrim=prim} = primDontKill prim

-- | A helper function to read a 'Net' from a 'MNetlist'
readNet :: InstId -> MNetlistPass s Net
readNet instId ctxtRef = do
  MNetlistPassCtxt {..} <- readSTRef ctxtRef
  fromMaybe (error "encountered InstId with no matching Net")
            <$> readArray mnpNetlist instId

-- | A helper type for 'Net' reference counting
type NetCounts s = STUArray s InstId Int

-- | A 'Net' reference counting helper function
countNetRef :: MNetlistPass s (NetCounts s)
countNetRef ctxtRef = do
  MNetlistPassCtxt{..} <- readSTRef ctxtRef
  bounds <- getBounds mnpNetlist
  refCounts <- newArray bounds 0
  es <- getElems mnpNetlist
  -- count references for each Net
  let innerCount (InputWire (instId, _)) = do
        cnt <- readArray refCounts instId
        writeArray refCounts instId (cnt + 1)
      innerCount (InputTree _ inpts) = mapM_ innerCount inpts
  forM_ [e | Just e <- es] $ \net -> mapM_ innerCount (netInputs net)
  -- return reference counts
  return refCounts

-- | 'MNetlistPass' pruning the netlist, dropping all 'Nothing' entries and
--   remapping the remaining 'Net's' 'InstId's appropriately.
prune :: MNetlistPass s ()
prune ctxtRef = do
  mnl <- mnpNetlist <$> readSTRef ctxtRef -- expose the 'MNetlist'
  pairs <- catMaybes . (liveNet <$>) <$> getAssocs mnl
  let remap old =
        fromMaybe (error "Blarney.Netlist.Passes.Prune")
                  (lookup old $ zipWith (\(i, _) i' -> (i, i')) pairs [0..])
  pruned <- newListArray (0, length pairs - 1)
                         (Just <$> remapNetInstId remap <$> snd <$> pairs)
  writeSTRef ctxtRef MNetlistPassCtxt{ mnpNetlist = pruned }
  where liveNet (i, Just n) = Just (i, n)
        liveNet _ = Nothing

-- | Repeat computation until a predicate holds
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM pred act =
  act >>= \x -> if pred x then return x else untilM pred act

-- | Same as 'untilM' but discard the final result
untilM_ :: Monad m => (a -> Bool) -> m a -> m ()
untilM_ pred act = do untilM pred act
                      return ()
