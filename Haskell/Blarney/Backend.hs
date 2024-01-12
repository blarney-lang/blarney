{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE NoRebindableSyntax  #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
, writeVerilog
, writeVerilogModule
, writeVerilogTop
  -- * SMT backend
, module Blarney.Backend.SMT
, writeSMTScript
, verifyWith
  -- * Simulation backend
, module Blarney.Backend.Simulation
, simulate
, simulateCapture
, view
, viewFor
) where

import Prelude
import Data.Map ((!), toList, fromList)
import Data.IORef

import Blarney.Core.Bit
import Blarney.Core.Interface (Modular(..), makeModule)
import Blarney.Core.IfThenElse
import Blarney.Core.Flatten (ToNetlist(..))
import Blarney.Core.FShow
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
writeVerilogModule mod modName dirName =
  onNetlists mod modName \nls ->
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
  onNetlists mod modName \nls ->
    sequence_ [ if name /= modName then genVerilogModule nl name dirName
                                   else genVerilogTop    nl name dirName
              | (name, nl) <- toList nls ]

-- | Shorthand for 'writeVerilogModule', with output directory set to
-- current directory.
writeVerilog :: Modular a => String -> a -> IO ()
writeVerilog modName mod = do
  putStr ("Writing \"" ++ filename ++ "\"... ")
  writeVerilogModule mod modName "."
  putStrLn "done."
  where
    filename = modName ++ ".v"
  

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
  onNetlists circuit scriptName \nls ->
    -- XXX maybe do not consider all nested Netlists?
    sequence_ [ genSMTScript conf nl name dirName | (name, nl) <- toList nls ]

-- | This function interacts with an SMT solver to verify each assertion present
--   in 'circuit', introduced by calls to the 'assert' function.
verifyWith :: Modular a
           => VerifyConf -- ^ Verification configuration setup
           -> a          -- ^ Blarney circuit
           -> IO ()
verifyWith conf circuit =
  onNetlists circuit "circuit under verification" \nls ->
    -- XXX maybe do not consider all nested Netlists?
    sequence_ [ verifyWithSMT conf nl | (name, nl) <- toList nls ]

-- Simulation backend
--------------------------------------------------------------------------------

-- | Simulate the provided module
simulateCore :: (String -> IO ()) -> Module () -> IO ()
simulateCore puts circuit = do
  topSim <- onNetlists circuit topSimName \nls -> mdo
    sims <- fromList <$> sequence [ (,) name <$> compileSim sims nl puts
                                  | (name, nl) <- toList nls ]
    return $ sims ! topSimName
  runSim topSim mempty
  return ()
  where
    topSimName = "circuit under simulation"

-- | Simulate the provided module
simulate :: Module () -> IO ()
simulate = simulateCore putStr

-- | Simulate the provided module, capturing the displayed output
simulateCapture :: Module () -> IO String
simulateCapture circuit = do
  log :: IORef [String] <- newIORef []
  simulateCore (\s -> modifyIORef log (s:)) circuit
  lines <- readIORef log
  return (concat $ reverse lines)

-- | Display the value of the given expression for 1 cycle
view :: FShow a => a -> IO ()
view expr =
  simulate do
    always do
      display expr
      finish

-- | Display the value of the given expression for n cycles
viewFor :: FShow a => Int -> a -> IO ()
viewFor n expr =
  simulate do
    count :: Reg (Bit 32) <- makeReg 1
    always do
      display expr
      count <== count.val + 1
      when (count.val .==. fromIntegral n) finish
