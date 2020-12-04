{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE NoTypeFamilies #-}

{-|
Module      : Blarney.Netlist.Passes
Description : Blarney netlist passes
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes (
  runNetlistPass
, runDefaultNetlistPasses
, wrapWithMandatoryNetlistPasses
, optionalNetlistPasses
, module Blarney.Netlist.Passes.Utils
, module Blarney.Netlist.Passes.ZeroWidthNetIgnore
, module Blarney.Netlist.Passes.ConstantFold
, module Blarney.Netlist.Passes.ConstantPropagate
, module Blarney.Netlist.Passes.ConstantEliminate
, module Blarney.Netlist.Passes.NetInline
, module Blarney.Netlist.Passes.NamePropagate
, module Blarney.Netlist.Passes.DeadNetEliminate
) where

import Prelude
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray (newArray, thaw, freeze)

import Data.STRef

import Blarney.Core.Opts
import Blarney.Netlist.Passes.Utils
import Blarney.Netlist.Passes.ZeroWidthNetIgnore
import Blarney.Netlist.Passes.ConstantFold
import Blarney.Netlist.Passes.ConstantPropagate
import Blarney.Netlist.Passes.ConstantEliminate
import Blarney.Netlist.Passes.NetInline
import Blarney.Netlist.Passes.NamePropagate
import Blarney.Netlist.Passes.DeadNetEliminate

-- Netlist transformation passes
--------------------------------------------------------------------------------

-- | Wrap a custom pass with the mandatory netlist transformation passes
wrapWithMandatoryNetlistPasses :: MNetlistPass s _ -> MNetlistPass s ()
wrapWithMandatoryNetlistPasses customPass mnl = do
  -- remove 'Bit 0' instances
  zeroWidthNetIgnore mnl
  -- run custom netlist pass
  customPass mnl
  -- eliminate 'Net' entries in the netlist for 'Net's that are no longer
  -- referenced
  untilM_ not $ deadNetEliminate mnl

---- | Netlist pass combining optional passes
optionalNetlistPasses :: Opts -> MNetlistPass s ()
optionalNetlistPasses opts mnl = do
  -- netlist optimisation passes
  when (optEnableSimplifier opts) do constantEliminate mnl
                                     singleRefNetInline mnl
                                     return ()
  -- propagates existing names through the netlist
  when (optEnableNamePropagation opts) $ namePropagate mnl

-- | Run an 'MNetlistPass' on a 'Netlist' and return the resulting 'Netlist'
runNetlistPass :: (forall s. MNetlistPass s _) -> Netlist -> Netlist
runNetlistPass pass netlist = runST m
  where m :: ST s Netlist
        m = do -- get a mutable netlist
               mnl <- thaw netlist
               -- apply netlist transformations
               pass mnl
               -- return transformed netlist as immutable
               freeze mnl

-- | Run the default set of netlist passes
runDefaultNetlistPasses :: Opts -> Netlist -> Netlist
runDefaultNetlistPasses opts netlist = runNetlistPass pass netlist
  where pass :: MNetlistPass s _
        pass = wrapWithMandatoryNetlistPasses $ optionalNetlistPasses opts
