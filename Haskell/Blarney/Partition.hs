{- Netlist partitioning functions for parallel simulation -}

module Blarney.Partition 
  ( partition
  ) where

import Prelude
import Blarney.Unbit
import Blarney.DataFlow

{-

Basic partitioning
------------------

This is the most basic partitioning scheme I can think of:

  1. Split the roots of the netlist into n sub-lists in textual
     order, where n is the number of hardware threads available
     for simulation.  (See Blarney.DataFlow for the definition of
     netlist roots.)

  2. Apply the 'Blarney.DataFlow.postOrder' pass to each sub-list.
     This expands each sub-list to include all combinatorial logic
     that feeds in to any primitive of the original sub-list.
     Some combinatorial logic may be replicated, i.e. present in many
     sub-lists, and hence be executed by several threads, but this
     avoids the need for sub-cycle synchronisation.

-}

-- Split list into n sub-lists
splitInto :: Int -> [a] -> [[a]]
splitInto n xs = split n xs
  where
    groupLen   = (length xs + n-1) `div` n
    split 0 xs = []
    split n [] = replicate n []
    split n xs = take groupLen xs : split (n-1) (drop groupLen xs)

partition :: Int -> [Net] -> [[Net]]
partition n nets = map (postOrder netArray) (splitInto n roots)
  where
    netArray = buildNetArray nets
    roots    = concatMap (getRoot netArray) nets
