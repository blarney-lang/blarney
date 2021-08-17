{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Netlist
Description : Netlist module for the blarney hardware description library
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module exports some utilities to work with 'Blarney' 'Netlists'. It is
typically omported by code in modules such as 'Blarney.Backend'.

-}

module Blarney.Netlist (
-- * Default set of 'Netlist' transformations
  runDefaultNetlistPasses
-- * 'Netlist' transformation passes ('MNetlistPass') helpers
, runNetlistPass
, optionalNetlistPasses
, wrapWithMandatoryNetlistPasses
-- * Exports of individual 'MNetlistPass'es and other utils
, module Blarney.Netlist.Passes
) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray

import Data.STRef

import Blarney.Misc.MonadLoops
import Blarney.Core.Opts
import Blarney.Netlist.Passes

-- | 'ToNetlist' instance for 'MNetlistRef's in the 'ST' monad
instance ToNetlist (MNetlistRef s) (ST s) where
  toNetlist mnlRef = do
    -- prune the netlist of all 'Nothing' entries
    prune mnlRef
    -- extract the mutable netlist and freeze it, dropping the Maybe wrapping
    mnl <- readSTRef mnlRef
    x <- mapArray (fromMaybe $ error "Blarney.Netlist.Passes.Utils:\
                                     \toNetlist encountered non Just entry\
                                     \after a netlist pruning")
                  mnl
    freeze x

-- | Wrap a custom pass with the mandatory netlist transformation passes
wrapWithMandatoryNetlistPasses :: MNetlistPass s a -> MNetlistPass s ()
wrapWithMandatoryNetlistPasses customPass mnlRef = do
  -- remove 'Bit 0' instances
  zeroWidthNetIgnore mnlRef
  -- run custom netlist pass
  customPass mnlRef
  -- eliminate 'Net' entries in the netlist for 'Net's that are no longer
  -- referenced
  deadNetEliminate mnlRef `untilPredM_` not

---- | Netlist pass combining optional passes
optionalNetlistPasses :: Opts -> MNetlistPass s ()
optionalNetlistPasses opts mnlRef = do
  -- netlist optimisation passes
  when (optEnableSimplifier opts) do constantEliminate mnlRef
                                     singleRefNetInline mnlRef
                                     return ()
  -- propagates existing names through the netlist
  when (optEnableNamePropagation opts) $ namePropagate mnlRef
  -- XXX De-inline don't cares (Workaround issue in verilator.
  -- XXX Avoid if possible)
  when (optEnableDontCareDeInline opts) $ dontCareDeInline mnlRef >> return ()

-- | Run an 'MNetlistPass' on a 'Netlist' and return the resulting 'Netlist'
runNetlistPass :: (forall s. MNetlistPass s a) -> Netlist -> Netlist
runNetlistPass pass netlist = runST wrappedPass
  where wrappedPass :: ST s Netlist
        wrappedPass = do
          -- get a mutable netlist
          mnl <- thaw $ amap Just netlist
          -- create a MNetlistPassCtxt to run passes on
          mnlRef <- newSTRef mnl
          -- apply netlist transformations
          pass mnlRef
          -- return transformed netlist as immutable
          toNetlist mnlRef

-- | Run the default set of netlist passes
runDefaultNetlistPasses :: Opts -> Netlist -> Netlist
runDefaultNetlistPasses opts netlist = runNetlistPass pass netlist
  where pass :: MNetlistPass s ()
        pass = wrapWithMandatoryNetlistPasses $ optionalNetlistPasses opts
