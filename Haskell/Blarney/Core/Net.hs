{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoRebindableSyntax  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Blarney.Core.Net
Description : Net primitive for Netlist construction
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides types and functions to represent circuits as 'Netlist's
that can then be rendered as Verilog or in other formats...

-}

module Blarney.Core.Net (
  Net(..)         -- 'Net' type to represent 'Netlist' nodes
, WireId          -- 'WireId' type to uniquely identify wires
, NetInput(..)    -- 'NetInput' type to represent inputs to 'Net's
, netInputWireIds -- Helper function to extract all 'NetInput''s 'WireId's
, Netlist         -- 'Netlist' type to represent a circuit
, getNet          -- Extract the 'Net' from a 'Netlist' at the provided 'InstId'
, topologicalSort -- Topologically sort a 'Netlist'
, partialTopologicalSort -- Topologically sort a subset of a 'Netlist' from a
                         -- single given root
) where

import Prelude
import Data.Maybe
import Data.Array
import Data.STRef
import Data.Array.ST
import Control.Monad.ST

import Blarney.Core.Prim

-- General type definitions and helpers
--------------------------------------------------------------------------------

-- | 'Net' type representing a 'Netlist' node
data Net = Net { -- | The 'Net' 's 'Prim'itive
                 netPrim         :: Prim
                 -- | The 'Net' 's 'InstId' identifier
               , netInstId       :: InstId
                 -- | The 'Net' 's list of 'NetInput' inputs
               , netInputs       :: [NetInput]
                 -- | The 'Net' 's 'NameHints'
               , netNameHints    :: NameHints
               } deriving Show

-- | A 'WireId' uniquely identify a wire with a 'Net''s instance identifier
--   ('InstId') and an output name ('OutputName')
type WireId = (InstId, OutputName)

-- | A 'Net''s input ('NetInput') can be:
--   - a wire, using the 'InputWire' constructor
--   - a complex expression, using the 'InputTree' constructor
data NetInput = InputWire WireId
              | InputTree Prim [NetInput]
              deriving Show

-- | Helper function to extract all 'NetInput''s 'WireId's
netInputWireIds :: NetInput -> [WireId]
netInputWireIds (InputWire wId) = [wId]
netInputWireIds (InputTree _ ins) = concatMap netInputWireIds ins

-- | A 'Netlist', represented as an 'Array InstId (Maybe Net)'
type Netlist = Array InstId (Maybe Net)

-- | Extract the 'Net' from a 'Netlist' at the provided 'InstId'. Raise an error
--   if no 'Net' with this 'InstId' is present.
getNet :: Netlist -> InstId -> Net
getNet nl i = fromMaybe err (nl ! i)
  where err = error $ "Blarney.Core.Net: " ++
                      "access to non existing Net at instId " ++ show i

-- topological stort of a netlist
--------------------------------------------------------------------------------

-- | the 'Mark' type is only useful to the 'topologicalSort' function and should
--   not be exported
data Mark = Unmarked | Temporary | Permanent

-- | get a topologically sorted '[InstId]' for the given 'Netlist'
topologicalSort :: Netlist -> [InstId]
topologicalSort nl = topoSort nl relevantRoots
  where relevantRoots = [ netInstId n | Just n@Net{netPrim = p} <- elems nl
                                      , case p of Output _ _ -> True
                                                  Assert _   -> True
                                                  _          -> False ]

-- | get a partially topologically sorted '[InstId]' for the subset of the given
--   'Netlist' from the given 'Net''s 'InstId'
partialTopologicalSort :: Netlist -> InstId -> [InstId]
partialTopologicalSort nl instId = topoSort nl [instId]

-- | internalhelper for topological sort of 'Netlist's
topoSort :: Netlist -> [InstId] -> [InstId]
topoSort nl rootIds = runST do
  -- initialise state for the topological sorting
  visited <- newArray (bounds nl) Unmarked -- track visit through the netlist
  sorted  <- newSTRef [] -- sorted list as a result
  roots  <- newSTRef rootIds -- list of roots to explore next
  -- run the internal topological sort while there are roots to explore
  whileM_ (notEmpty roots) do
    root <- pop roots -- consume a root
    go visited sorted roots root -- explore from the consumed root
  -- return the sorted list of InstId
  reverse <$> readSTRef sorted
  -- helpers
  where
    whileM_ :: Monad m => m Bool -> m a -> m ()
    whileM_ pred act = pred >>= \x -> if x then act >> whileM_ pred act
                                           else return ()
    -- identify leaf net
    isLeaf :: Net -> Bool
    isLeaf Net{ netPrim = Input      _ _ } = True
    isLeaf Net{ netPrim = RegisterEn _ _ } = True
    isLeaf Net{ netPrim = Register   _ _ } = True
    isLeaf _                               = False
    -- helpers to use an STRef [a] as a stack
    push stck elem = modifySTRef' stck $ \xs -> elem : xs
    pushN stck elems = modifySTRef' stck $ \xs -> elems ++ xs
    pop stck = do top <- head <$> readSTRef stck
                  modifySTRef' stck $ \xs -> tail xs
                  return top
    notEmpty stck = do l <- readSTRef stck
                       return (not . null $ l)
    -- the actual recursive topological sort algorithm
    go :: STArray s InstId Mark -> STRef s [InstId] -> STRef s [InstId]
       -> InstId
       -> ST s ()
    go visited sorted roots netId = do
      let net = getNet nl netId
      -- retrieve the 'visited' array entry for the current net
      netVisit <- readArray visited netId
      case netVisit of
        -- For already visited nets, do not do anything
        Permanent -> return ()
        -- For nets under visit, we identified a combinational cycle
        -- (unsupported ==> error out)
        Temporary -> error $ "Blarney.Core.Net: " ++
                             "combinational cycle detected -- " ++ show net
        -- For new visits:
        Unmarked -> do
          let allInputIds = concatMap (map fst . netInputWireIds)
                                      (netInputs net)
          -- For leaf nets, strop here and mark inputs as roots for next
          -- toplevel call
          if isLeaf net then do pushN roots allInputIds
          -- For non leaf nets, mark net as temporarily under visit and explore
          -- all inputs recursively
          else do writeArray visited netId Temporary
                  mapM_ (go visited sorted roots) allInputIds
          -- Mark net as permanent and insert it into the sorted list
          writeArray visited netId Permanent
          push sorted netId
