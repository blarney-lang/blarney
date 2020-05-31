{-|
Module      : Blarney.Netlist.Passes
Description : Blarney netlist passes
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes (
  mandatoryNetlistPasses
, optionalNetlistPasses
, defaultNetlistPasses
) where

import Prelude
import Control.Monad
import Data.Array.MArray (freeze)

import Blarney.Core.Opts
import Blarney.Netlist.Utils
import Blarney.Netlist.Passes.Utils
import Blarney.Netlist.Passes.ZeroWidthNetIgnore
import Blarney.Netlist.Passes.ConstantFold
import Blarney.Netlist.Passes.ConstantPropagate
import Blarney.Netlist.Passes.NetInline
import Blarney.Netlist.Passes.NamePropagate
import Blarney.Netlist.Passes.DeadNetEliminate

-- Netlist transformation passes
--------------------------------------------------------------------------------

-- | A constant elimination pass
constantEliminate :: MNetlistPass ()
constantEliminate mnl = untilM not constElim >> return ()
                        where constElim = do a <- constantFold mnl
                                             b <- constantPropagate mnl
                                             return $ a || b

-- | Run mandatory set of netlist transformation passes and a (potentially
--   empty) custom pass
mandatoryNetlistPasses :: MNetlistPass _ -> MNetlistPass Netlist
mandatoryNetlistPasses customPass mnl = do
  -- remove 'Bit 0' instances
  zeroWidthNetIgnore mnl
  -- run custom netlist pass
  customPass mnl
  -- eliminate 'Net' entries in the netlist for 'Net's that are no longer
  -- referenced
  untilM not $ deadNetEliminate mnl
  -- turn the final netlist immutable
  freeze mnl

-- | Netlist pass combining optional passes
optionalNetlistPasses :: Opts -> MNetlistPass ()
optionalNetlistPasses opts mnl = do
  -- netlist optimisation passes
  when (optEnableSimplifier opts) do constantEliminate mnl
                                     singleRefNetInline mnl
                                     return ()
  -- propagates existing names through the netlist
  when (optEnableNamePropagation opts) $ namePropagate mnl

-- | The default set of netlist passes
defaultNetlistPasses :: Opts -> MNetlistPass Netlist
defaultNetlistPasses opts = mandatoryNetlistPasses $ optionalNetlistPasses opts
