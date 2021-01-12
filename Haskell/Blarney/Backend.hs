{-|
Module      : Blarney.Backend
Description : Backend module for the blarney hardware description library
Copyright   : (c) Matthew Naylor, 2020
              (c) Alexandre Joannou, 2020-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Backend (
  -- * Verilog backend
  module Blarney.Backend.Verilog
, writeVerilogModule
, writeVerilogTop
  -- * SMT backend
, module Blarney.Backend.SMT
, writeSMTScript
, verifyWith
) where

import Prelude

import Blarney.Core.Opts
import Blarney.Core.Interface (Modular(..), makeModule)
import Blarney.Core.Flatten (ToNetlist(..))
import Blarney.Core.Module
import Blarney.Netlist

import Blarney.Backend.Verilog
import Blarney.Backend.SMT

-- Verilog backend
--------------------------------------------------------------------------------

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
  nl <- toNetlist (makeModule mod)
  let nl' = runDefaultNetlistPasses opts nl
  genVerilogModule nl' modName dirName

-- | This function is similar to 'writeVerilogModule' but also generates
-- a verilator wrapper and Makefile.
-- This is useful for simple examples.  Major projects will
-- probably require a more customised verilator wrapper;
-- in that case, just use 'writeVerilogModule', even for the top-level
-- module, and write a custom verilator driver, perhaps using the
-- Blarney-generated one as a starting point.
writeVerilogTop :: Module () -- ^ Blarney function
                -> String    -- ^ Module name
                -> String    -- ^ Output directory
                -> IO ()
writeVerilogTop mod modName dirName = do
  (opts, _) <- getOpts
  nl <- toNetlist mod
  let nl' = runDefaultNetlistPasses opts nl
  genVerilogTop nl' modName dirName

-- SMT backend
--------------------------------------------------------------------------------

-- | This function generates an SMT script for the 'pred' Blarney predicate.
--   The name of the generated SMT script is specified with 'scriptName' and
--   the generated file is `'dirName'/'scriptName'.smt2`.
writeSMTScript :: Modular a
                => VerifyConf
                -> a      -- ^ Blarney predicate
                -> String -- ^ Script name
                -> String -- ^ Output directory
                -> IO ()
writeSMTScript conf pred scriptName dirName = do
  (opts, _) <- getOpts
  nl <- toNetlist $ makeModule pred
  let nl' = runDefaultNetlistPasses opts nl
  genSMTScript conf nl' scriptName dirName

verifyWith :: Modular a
           => VerifyConf
           -> a      -- ^ Blarney predicate
           -> IO ()
verifyWith conf pred = do
  (opts, _) <- getOpts
  nl <- toNetlist . makeModule $ pred
  let nl' = runDefaultNetlistPasses opts nl
  verifyWithSMT conf nl'
