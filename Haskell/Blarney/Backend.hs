{-# LANGUAGE NoRebindableSyntax #-}

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
import Data.Map ((!), toList, fromList)

import Blarney.Core.Interface (Modular(..), makeModule)
import Blarney.Core.Flatten (ToNetlist(..))
import Blarney.Core.Module
import Blarney.Netlist

import Blarney.Backend.Simulation
import Blarney.Backend.Verilog
import Blarney.Backend.SMT
import Blarney.Backend.Utils

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
writeVerilogModule mod modName dirName =
  runWithElaboratedHierarchy mod modName \nls ->
    sequence_ [ genVerilogModule nl name dirName | (name, nl) <- toList nls ]

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
writeVerilogTop mod modName dirName =
  runWithElaboratedHierarchy mod modName \nls ->
    sequence_ [ if name /= modName then genVerilogModule nl name dirName
                                   else genVerilogTop    nl name dirName
              | (name, nl) <- toList nls ]

-- SMT backend
--------------------------------------------------------------------------------

-- | This function generates an SMT script to verify each assertion present in
--   'circuit', introduced by calls to the 'assert' function.
--   The name of the generated SMT script is specified with 'scriptName' and
--   the generated file is `'dirName'/'scriptName'.smt2`.
writeSMTScript :: Modular a
                => VerifyConf -- ^ Verification configuration setup
                -> a          -- ^ Blarney circuit
                -> String     -- ^ Script name
                -> String     -- ^ Output directory
                -> IO ()
writeSMTScript conf circuit scriptName dirName =
  runWithElaboratedHierarchy circuit scriptName \nls ->
    -- XXX maybe do not consider all nested Netlists?
    sequence_ [ genSMTScript conf nl name dirName | (name, nl) <- toList nls ]

-- | This function interacts with an SMT solver to verify each assertion present
--   in 'circuit', introduced by calls to the 'assert' function.
verifyWith :: Modular a
           => VerifyConf -- ^ Verification configuration setup
           -> a          -- ^ Blarney circuit
           -> IO ()
verifyWith conf circuit =
  runWithElaboratedHierarchy circuit "circuit under verification" \nls ->
    -- XXX maybe do not consider all nested Netlists?
    sequence_ [ verifyWithSMT conf nl | (name, nl) <- toList nls ]

-- Simulation backend
--------------------------------------------------------------------------------

-- | Simulate the provided Blarney circuit
simulate :: Modular a
         => a     -- ^ Blarney circuit
         -> IO ()
simulate circuit = do
  topSim <- runWithElaboratedHierarchy circuit topSimName \nls -> mdo
    sims <- fromList <$> sequence [ compileSim sims nl >>= return . (,) name
                                  | (name, nl) <- toList nls ]
    return $ sims ! topSimName
  runSim topSim mempty
  where topSimName = "circuit under simulation"
