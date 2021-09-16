{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Netlist
Description : Netlist module for the blarney hardware description library
Copyright   : (c) Alexandre Joannou, 2020-2021
              (c) Matthew Naylor, 2021
License     : MIT
Stability   : experimental

This module exports some utilities to work with 'Blarney' 'Netlists'. It is
typically omported by code in modules such as 'Blarney.Backend'.

-}

module Blarney.Netlist (
-- * Default set of 'Netlist' transformations
  runDefaultNetlistPasses
-- * 'Netlist' transformation passes ('MNetlistPass') helpers
, runNetlistPass
, optionalNetlistPasses
, wrapWithMandatoryNetlistPasses
-- * Exports of individual 'MNetlistPass'es and other utils
, module Blarney.Netlist.Passes
-- * others
, onNetlists
) where

import Prelude
import Data.Maybe
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Map (Map, member, notMember, insert)

import Blarney.Core.Opts
import Blarney.Core.Interface
import Blarney.Netlist.Passes
import Blarney.Misc.MonadLoops

freezeMNetlist :: MNetlistRef s -> ST s Netlist
freezeMNetlist mnlRef = do
  -- prune the netlist of all 'Nothing' entries
  prune mnlRef
  -- extract the mutable netlist and freeze it, dropping the Maybe wrapping
  mnl <- readSTRef mnlRef
  x <- mapArray (fromMaybe $ error "Blarney.Netlist.Passes.Utils:\
                                     \freezeMNetlist encountered non Just entry\
                                     \after a netlist pruning")
                  mnl
  freeze x

-- | Wrap a custom pass with the mandatory netlist transformation passes
wrapWithMandatoryNetlistPasses :: MNetlistPass s a -> MNetlistPass s ()
wrapWithMandatoryNetlistPasses customPass mnlRef = do
  -- remove 'Bit 0' instances
  zeroWidthNetIgnore mnlRef
  -- run custom netlist pass
  customPass mnlRef
  -- eliminate 'Net' entries in the netlist for 'Net's that are no longer
  -- referenced
  deadNetEliminate mnlRef `untilPredM_` not

---- | Netlist pass combining optional passes
optionalNetlistPasses :: Opts -> MNetlistPass s ()
optionalNetlistPasses opts mnlRef = do
  -- netlist optimisation passes
  when (optEnableSimplifier opts) do constantEliminate mnlRef
                                     singleRefNetInline mnlRef
                                     return ()
  -- propagates existing names through the netlist
  when (optEnableNamePropagation opts) $ namePropagate mnlRef
  -- XXX De-inline don't cares (Workaround issue in verilator.
  -- XXX Avoid if possible)
  when (optEnableDontCareDeInline opts) $ dontCareDeInline mnlRef >> return ()

-- | Run an 'MNetlistPass' on a 'Netlist' and return the resulting 'Netlist'
runNetlistPass :: (forall s. MNetlistPass s a) -> Netlist -> Netlist
runNetlistPass pass netlist = runST wrappedPass
  where wrappedPass :: ST s Netlist
        wrappedPass = do
          -- get a mutable netlist
          mnl <- thaw $ amap Just netlist
          -- create a MNetlistPassCtxt to run passes on
          mnlRef <- newSTRef mnl
          -- apply netlist transformations
          pass mnlRef
          -- return transformed netlist as immutable
          freezeMNetlist mnlRef

-- | Run the default set of netlist passes
runDefaultNetlistPasses :: Opts -> Netlist -> Netlist
runDefaultNetlistPasses opts netlist = runNetlistPass pass netlist
  where pass :: MNetlistPass s ()
        pass = wrapWithMandatoryNetlistPasses $ optionalNetlistPasses opts

-- | Run an 'IO' function with the elaborated 'Netlist's hierarchy for the
--   given circuit (effectively one 'Netlist' per eligible 'Custom' 'Net'). The
--   'Netlist's are passed to the provided function as a 'Map String Netlist',
--   with the toplevel 'Netlist''s key being the provided 'String' argument, and
--   the other 'Netlist's using the 'customName' field of the elaborated
--   'Custom' as a key.
onNetlists :: Modular a
           => a                            -- ^ Blarney circuit
           -> String                       -- ^ circuit name
           -> (Map String Netlist -> IO b) -- ^ function to run
           -> IO b
onNetlists circuit name f = do
  let nl0 = toNetlist $ makeModule circuit
  nls <- elab mempty [(name, nl0)]
  f nls
  where elab :: Map String Netlist  -- ^ Accumlator of explored modules
             -> [(String, Netlist)] -- ^ List of unexplored modules
             -> IO (Map String Netlist)
        elab acc [] = return acc
        elab acc ((name, nl0):rest)
          | name `member` acc = elab acc rest
          | otherwise = do
              (opts, _) <- getOpts
              -- Run netlist generator
              let nl = runDefaultNetlistPasses opts nl0
              -- Insert resulting netlist into accumulator
              let newAcc = insert name nl acc
              -- Look for new netlists
              elab newAcc $
                [ (nm, nl')
                | Custom { customName = nm
                         , customNetlist = Just (CustomNetlist nl') } <-
                    map netPrim (elems nl)
                , nm `notMember` newAcc
                ] ++ rest
