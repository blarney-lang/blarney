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

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- | 'Netlist' pruning pass
prune :: MNetlistPass s ()
prune mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  assocs <- getAssocs mnl
  let pairs = [(i, n) | (i, Just n) <- assocs]
  let remap old =
        fromMaybe (error "Blarney.Netlist.Passes.Prune")
                  (lookup old $ zipWith (\(i, _) i' -> (i, i')) pairs [0..])
  pruned <- newListArray (0, length pairs - 1)
                         (Just <$> remapNetInstId remap <$> snd <$> pairs)
  writeSTRef mnlRef pruned
