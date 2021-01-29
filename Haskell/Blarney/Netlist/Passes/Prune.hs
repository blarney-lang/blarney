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
import Data.Array.IArray

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- | 'Netlist' pruning pass
prune :: MNetlistPass s ()
prune mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  assocs <- getAssocs mnl
  let pairs = [(i, n) | (i, Just n) <- assocs]
  -- Mapping from old ids to new ids
  (_, maxOldId) <- getBounds mnl
  let mapping :: Array Int Int =
        array (0, maxOldId) (zip (map fst pairs) [0..])
  -- Compute compact netlist
  let maxNewId = length pairs - 1
  pruned <- newListArray (0, maxNewId)
    [Just (remapNetInstId (mapping !) net) | (_, net) <- pairs]
  writeSTRef mnlRef pruned
