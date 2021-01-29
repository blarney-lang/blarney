{- |
Module      : Blarney.Netlist.Passes.Prune
Description : A blarney netlist pass to prune a netlist
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

-- | 'MNetlistPass' pruning the netlist, dropping all 'Nothing' entries and
--   remapping the remaining 'Net's' 'InstId's appropriately.

-}

module Blarney.Netlist.Passes.Prune (
  prune
) where

import Prelude
import Data.Maybe
import Data.STRef
import Data.Array.ST
import Data.Array.Unboxed

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- | 'Netlist' pruning pass
prune :: MNetlistPass s ()
prune mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  assocs <- getAssocs mnl
  let usedAssocs = [(i, n) | (i, Just n) <- assocs]
  let oldIds = map fst usedAssocs
  let oldNets = map snd usedAssocs
  -- Mapping from old ids to new ids
  bounds <- getBounds mnl
  let mapping :: UArray InstId InstId =
        array bounds (zip oldIds [0..])
  -- Compute compact netlist
  pruned <- newListArray (0, length oldNets - 1)
                         [ Just (remapNetInstId (mapping !) net)
                         | net <- oldNets ]
  writeSTRef mnlRef pruned
