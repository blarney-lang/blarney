{-|
Module      : Blarney.Backend
Description : Backend module for the blarney hardware description library
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Backend (
  -- * Verilog backend
  module Blarney.Backend.Verilog
, writeVerilogModule
, writeVerilogTop
) where

import Prelude

import Blarney.Core.Opts
import Blarney.Core.Interface (Modular(..), makeModule)
import Blarney.Core.Flatten (ToMNetlist(..))
import Blarney.Netlist

-- Verilog backend
--------------------------------------------------------------------------------

import Blarney.Backend.Verilog

-- | This function generates Verilog for the 'mod' Blarney 'Modular a' function.
--   The name of the generated Verilog module is specified with 'modName' and
--   the generated Verilog file is `'dirName'/'modName'.v`.
writeVerilogModule :: Modular a
                   => a      -- ^ Blarney function
                   -> String -- ^ Module name
                   -> String -- ^ Output directory
                   -> IO ()
writeVerilogModule mod modName dirName = do
  (opts, _) <- getOpts
  mnl <- toMNetlist $ makeModule mod
  nl <- defaultNetlistPasses opts mnl
  genVerilogModule nl modName dirName

-- This function is similar to 'writeVerilogModule' but also generate simulation
-- files and Makefiles for toplevel modules
writeVerilogTop :: Modular a
                => a      -- ^ Blarney function
                -> String -- ^ Module name
                -> String -- ^ Output directory
                -> IO ()
writeVerilogTop mod modName dirName = do
  (opts, _) <- getOpts
  mnl <- toMNetlist $ makeModule mod
  nl <- defaultNetlistPasses opts mnl
  genVerilogTop nl modName dirName
