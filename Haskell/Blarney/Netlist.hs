{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE BlockArguments #-}

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
, module Blarney.Netlist.Utils
, module Blarney.Netlist.Passes
) where

import Prelude
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray (amap)
import Data.Array.MArray (newArray, thaw, freeze)

import Data.STRef

import Blarney.Core.Opts
import Blarney.Netlist.Utils
import Blarney.Netlist.Passes

-- | Wrap a custom pass with the mandatory netlist transformation passes
wrapWithMandatoryNetlistPasses :: MNetlistPass s a -> MNetlistPass s ()
wrapWithMandatoryNetlistPasses customPass ctxtRef = do
  -- remove 'Bit 0' instances
  zeroWidthNetIgnore ctxtRef
  -- run custom netlist pass
  customPass ctxtRef
  -- eliminate 'Net' entries in the netlist for 'Net's that are no longer
  -- referenced
  untilM_ not $ deadNetEliminate ctxtRef

---- | Netlist pass combining optional passes
optionalNetlistPasses :: Opts -> MNetlistPass s ()
optionalNetlistPasses opts ctxtRef = do
  -- netlist optimisation passes
  when (optEnableSimplifier opts) do constantEliminate ctxtRef
                                     singleRefNetInline ctxtRef
                                     return ()
  -- propagates existing names through the netlist
  when (optEnableNamePropagation opts) $ namePropagate ctxtRef

-- | Run an 'MNetlistPass' on a 'Netlist' and return the resulting 'Netlist'
runNetlistPass :: (forall s. MNetlistPass s a) -> Netlist -> Netlist
runNetlistPass pass netlist = runST wrappedPass
  where wrappedPass :: ST s Netlist
        wrappedPass = do
          -- get a mutable netlist
          mnl <- thaw $ amap Just netlist
          -- create a MNetlistPassCtxt to run passes on
          ctxtRef <- newSTRef MNetlistPassCtxt{ mnpNetlist = mnl }
          -- apply netlist transformations
          pass ctxtRef
          -- return transformed netlist as immutable
          toNetlist ctxtRef

-- | Run the default set of netlist passes
runDefaultNetlistPasses :: Opts -> Netlist -> Netlist
runDefaultNetlistPasses opts netlist = runNetlistPass pass netlist
  where pass :: MNetlistPass s ()
        pass = wrapWithMandatoryNetlistPasses $ optionalNetlistPasses opts
