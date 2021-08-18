{-|
Module      : Blarney.Backend.Utils
Description : Utility functions and types shared accross backends
Copyright   : (c) Matthew Naylor, 2021
              (c) Alexandre Joannou, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Backend.Utils (
  elaborateHierarchy
, runWithElaboratedHierarchy
) where

import Prelude
import Data.Map (Map, member, notMember, insert)
import Data.Array (elems)

import Blarney.Netlist
import Blarney.Core.Opts
import Blarney.Core.Interface

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
      (opts, _) <- getOpts
      -- Run netlist generator
      nl <- runDefaultNetlistPasses opts <$> nlg
      -- Insert resulting netlist into accumulator
      let newAcc = insert name nl acc
      -- Look for new netlists
      elaborateHierarchy newAcc $
        [ (nm, getNetlistGenerator nlg')
        | Custom { customName = nm
                 , customNetlist = Just nlg' } <- map netPrim (elems nl)
        , nm `notMember` newAcc
        ] ++ rest

-- | Run an `IO` function with the elaborated netlist hierarchy for the given
--   circuit received as argument
runWithElaboratedHierarchy :: Modular a
                           => a                            -- ^ Blarney circuit
                           -> String                       -- ^ circuit name
                           -> (Map String Netlist -> IO b) -- ^ function to run
                           -> IO b
runWithElaboratedHierarchy circuit name f = do
  nl0 <- toNetlist $ makeModule circuit
  nls <- elaborateHierarchy mempty [(name, return nl0)]
  f nls
