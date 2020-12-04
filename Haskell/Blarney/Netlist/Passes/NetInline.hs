{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Netlist.Passes.NetInline
Description : A blarney netlist pass to inline a Net into an other Net's inputs
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.NetInline (
  singleRefNetInline
) where

import Prelude
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray

import Blarney.Netlist.Passes.Utils

-- | Helper to inline a 'Net''s inputs
netInputInline :: MNetlist s -> NetCounts s -> NetInput -> ST s (NetInput, Bool)
netInputInline mnl nc inpt@(InputWire (instId, _)) = do
  -- read ref count for our referenced 'Net'
  cnt <- readArray nc instId
  -- read netPrim and netInputs for our referenced 'Net'
  Net{ netPrim = prim, netInputs = inpts } <- readNet instId mnl
  -- attempt inlining our referenced 'Net', and its inputs recursively, also
  -- returning if inlining could happen as a Bool
  if cnt == 1 && canInline prim then do
    (inpts', changes) <- unzip <$> mapM
      (\x -> if canInlineInput prim then do (x', _) <- netInputInline mnl nc x
                                            return (x', True)
             else return (x, False)) inpts
    return (InputTree prim inpts', or changes)
  else return (inpt, False)
netInputInline mnl nc (InputTree prim inpts) = do
  (inpts', changes) <- unzip <$> mapM (netInputInline mnl nc) inpts
  return $ (InputTree prim inpts', or changes)

-- | Single reference 'Net' inlining pass
singleRefNetInline :: MNetlistPass s Bool
singleRefNetInline mnl = do
  refCounts <- countNetRef mnl -- reference count for each 'Net'
  pairs <- getAssocs mnl -- list of nets with their index
  changed <- newSTRef False -- keep track of modifications to the 'Netlist'
  -- Inline each "inlinable" (that is with a supported combinational underlying
  -- primitive) 'Net'
  forM_ [(a,b) | x@(a, Just b) <- pairs, canInlineInput (netPrim b)] $
    \(idx, net) -> do
      (netInputs', changes) <- unzip <$> mapM (netInputInline mnl refCounts)
                                              (netInputs net)
      when (or changes) $ do -- on change, update 'Netlist' and keep track
        writeArray mnl idx (Just net { netInputs = netInputs' })
        writeSTRef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readSTRef changed
  -- DEBUG HELP -- putStrLn $ "netInputInline pass changed? " ++ show x
  readSTRef changed
