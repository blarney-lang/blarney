{-|
Module      : Blarney.Netlist
Description : Netlist module for the blarney hardware description library
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental

This module re exports internal components to help other consuming code
(typically code in the 'Blarney.Backend' module) interact with Blarney Netlists
-}
module Blarney.Netlist (
  module Blarney.Netlist.Utils
, module Blarney.Netlist.Passes
) where

import Blarney.Netlist.Utils
import Blarney.Netlist.Passes
