{-|
Module      : Blarney.Netlist.Passes
Description : Blarney netlist passes
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module simply re-exports individual 'MNetlistPass'es.

-}

module Blarney.Netlist.Passes (
  module Blarney.Netlist.Passes.Types
, module Blarney.Netlist.Passes.Utils
, module Blarney.Netlist.Passes.Prune
, module Blarney.Netlist.Passes.NetInline
, module Blarney.Netlist.Passes.ConstantFold
, module Blarney.Netlist.Passes.NamePropagate
, module Blarney.Netlist.Passes.DeadNetEliminate
, module Blarney.Netlist.Passes.ConstantPropagate
, module Blarney.Netlist.Passes.ConstantEliminate
, module Blarney.Netlist.Passes.ZeroWidthNetIgnore
) where

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils
import Blarney.Netlist.Passes.Prune
import Blarney.Netlist.Passes.NetInline
import Blarney.Netlist.Passes.ConstantFold
import Blarney.Netlist.Passes.NamePropagate
import Blarney.Netlist.Passes.DeadNetEliminate
import Blarney.Netlist.Passes.ConstantPropagate
import Blarney.Netlist.Passes.ConstantEliminate
import Blarney.Netlist.Passes.ZeroWidthNetIgnore
