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
  -- * Simulation backend
, module Blarney.Backend.Simulation
, simulate
) where

import Prelude

import Blarney.Core.Opts
import Blarney.Core.Interface
import Blarney.Core.Flatten (ToNetlist(..))
import Blarney.Core.Module
import Blarney.Netlist

import Blarney.Backend.Simulation
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

-- | Generate verilog for given module and return a function that can
-- be used to instantiate that module
makeInstanceOf :: Modular a => a -> String -> Module a
makeInstanceOf m s = do
  addIO do
    writeVerilogModule m s "./"
  return (makeInstance s)

-- SMT backend
--------------------------------------------------------------------------------

-- | This function generates an SMT script to verify each assertion present in
--   'circuit', introduced by calls to the 'assert' function.
--   The name of the generated SMT script is specified with 'scriptName' and
--   the generated file is `'dirName'/'scriptName'.smt2`.
writeSMTScript :: Modular a
                => VerifyConf
                -> a      -- ^ Blarney circuit
                -> String -- ^ Script name
                -> String -- ^ Output directory
                -> IO ()
writeSMTScript conf circuit scriptName dirName = do
  (opts, _) <- getOpts
  nl <- toNetlist $ makeModule circuit
  let nl' = runDefaultNetlistPasses opts nl
  genSMTScript conf nl' scriptName dirName

-- | This function interacts with an SMT solver to verify each assertion present
--   in 'circuit', introduced by calls to the 'assert' function.
verifyWith :: Modular a
           => VerifyConf
           -> a      -- ^ Blarney circuit
           -> IO ()
verifyWith conf circuit = do
  (opts, _) <- getOpts
  nl <- toNetlist . makeModule $ circuit
  let nl' = runDefaultNetlistPasses opts nl
  verifyWithSMT conf nl'

-- Simulation backend
--------------------------------------------------------------------------------

-- | Simulation the provided Blarney circuit
simulate :: Modular a
         => a      -- ^ Blarney circuit
         -> IO ()
simulate circuit = do
  (opts, _) <- getOpts
  nl <- toNetlist $ makeModule circuit
  let nl' = runDefaultNetlistPasses opts nl
  simulateNetlist nl'
