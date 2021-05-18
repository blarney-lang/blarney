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
import Data.Map (Map, toList, fromList, member, notMember, insert, empty)
import Data.Array (elems)

import Blarney.Core.Opts
import Blarney.Core.Interface (Modular(..), makeModule)
import Blarney.Core.Flatten (ToNetlist(..))
import Blarney.Core.Module
import Blarney.Netlist

import Blarney.Backend.Simulation
import Blarney.Backend.Verilog
import Blarney.Backend.SMT

-- | Elaborate module hierarchy
elaborateHierarchy ::
     Map String Netlist
     -- ^ Accumlator of explored modules
  -> [(String, IO Netlist)]
     -- ^ List of currently unexplored modules
  -> IO (Map String Netlist)
elaborateHierarchy acc [] = return acc
elaborateHierarchy acc ((name, nlg):rest)
  | name `member` acc = elaborateHierarchy acc rest
  | otherwise = do
      -- Run netlist generator
      nl <- nlg
      -- Insert resulting netlist into accumulator
      let newAcc = insert name nl acc
      -- Look for new netlists
      elaborateHierarchy newAcc $
        [ (nm, getNetlistGenerator nlg)
        | Custom { customName = nm
                 , customNetlist = Just nlg } <- map netPrim (elems nl)
        , nm `notMember` newAcc
        ] ++ rest
        
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
  nls <- elaborateHierarchy empty [(modName, return nl)]
  sequence_
    [ do let nl' = runDefaultNetlistPasses opts nl
         genVerilogModule nl' name dirName
    | (name, nl) <- toList nls ]

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
  nls <- elaborateHierarchy empty [(modName, return nl')]
  sequence_
    [ do let nl' = runDefaultNetlistPasses opts nl
         genVerilogModule nl' name dirName
    | (name, nl) <- toList nls, name /= modName ]

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
