{- |
Module      : Blarney.Netlist.Passes.DontCareDeInline
Description : A blarney netlist pass to "de-inline" don't care primitives.
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This pass "de-inlines" 'DontCare' primitives that are otherwise inlined in an
'InputTree'. This undoes some of the work performed by the pass in the
'Blarney.Netlist.Passes.NetInline' module.
This pass exists to avoid backends generating code that might be too compact for
certain tools (specifically, verilator 4.202 cannot consume verilog that tries
to concatenate a don't care literal with a value literal).
This pass should be avoided if possible.

-}

module Blarney.Netlist.Passes.DontCareDeInline (
  dontCareDeInline
) where

import Prelude
import Data.List
import Data.STRef
import Control.Monad
import Data.Array.MArray

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- | 'DontCare' "de-inline"
dontCareDeInline :: MNetlistPass s Bool
dontCareDeInline mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  pairs <- getAssocs mnl -- list of nets with their index
  -- prepare a new 'DontCare' 'Net' generator
  (_, hi) <- getBounds mnl
  newInstId <- newSTRef $ hi + 1
  newDontCares <- newSTRef []
  let genNewDontCare w = do
        newId <- readSTRef newInstId
        modifySTRef' newInstId (+1)
        modifySTRef' newDontCares
                     (\xs -> Just (Net (DontCare w) newId [] mempty) : xs)
        return $ InputWire (newId, Nothing)
  --updateInputTree :: NetInput -> ST s NetInput
  let updateInputTree inpt = case inpt of
        InputWire _ -> return inpt
        InputTree (DontCare w) _ -> genNewDontCare w
        InputTree p ins -> do ins' <- mapM updateInputTree ins
                              return $ InputTree p ins'
  -- explore netlist for possible de-inline spots
  allExtracted <- forM [ (a, b) | x@(a, Just b) <- pairs ] $
    \(idx, net) -> do netInputs' <- mapM updateInputTree (netInputs net)
                      writeArray mnl idx $ Just net { netInputs = netInputs' }
  -- append all extracted 'DontCare' 'Net's to the netlist
  updatedOldNets <- getElems mnl
  newNets <- readSTRef newDontCares
  case newNets of
    [] -> return False
    Just Net { netInstId = newHi } : _ -> do
      newNets' <- newListArray (0, newHi) (updatedOldNets ++ reverse newNets)
      writeSTRef mnlRef newNets'
      return True
