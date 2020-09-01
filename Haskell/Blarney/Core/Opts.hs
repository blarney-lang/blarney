{-# LANGUAGE BlockArguments #-}

{-|
Module      : Blarney.Core.Opts
Description : Blarney options and command-line parser
Copyright   : (c) Matthew Naylor, 2020
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Core.Opts
  ( Opts(..)
  , defaultOpts
  , parseOpts
  , getOpts
  ) where

-- Standard imports
import Prelude
import System.Environment
import System.Console.GetOpt

-- Blarney options
data Opts =
  Opts {
    optEnableSimplifier :: Bool
  , optEnableNamePropagation :: Bool
  , optEnableVerilatorTrace :: Bool
  }
  deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts
  { optEnableSimplifier = False
  , optEnableNamePropagation = False
  , optEnableVerilatorTrace = False
  }

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option [] ["enable-simplifier"]
      (NoArg \opts -> opts { optEnableSimplifier = True })
      "Netlist simplification passes"
  , Option [] ["enable-name-prop"]
      (NoArg \opts -> opts { optEnableNamePropagation = True })
      "Name propagation pass"
  , Option [] ["enable-verilator-trace"]
      (NoArg \opts -> opts { optEnableVerilatorTrace = True })
      "Generate trace.vcd file"
  ]

-- Parse command line options
-- And return leftover (unrecognised) options
parseOpts :: [String] -> (Opts, [String])
parseOpts args =
  case getOpt' Permute options args of
    (opts, unused0, unused1, []) ->
      (foldl (flip id) defaultOpts opts, unused1 ++ unused0)
    (_, _, _, errs) ->
      error (concat errs ++ usageInfo "Blarney options: " options)

-- Extract options from the command line
-- And return leftover (unrecognised) options
getOpts :: IO (Opts, [String])
getOpts = parseOpts <$> getArgs
