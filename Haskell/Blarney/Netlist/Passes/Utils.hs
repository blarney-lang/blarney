{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Netlist.Passes.Utils
Description : Blarney netlist common pass utils
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.Utils (
  MNetlistPass   -- Type to capture a pass on a mutable netlist
, netIsRoot
, netDontKill
, readNet
, NetCounts
, countNetRef
, untilM
, module Blarney.Core.Net
, module Blarney.Core.Prim
) where

import Prelude
import Data.Maybe
import Control.Monad
import Data.Array.IO

import Blarney.Core.Net
import Blarney.Core.Prim

-- | A Pass type
type MNetlistPass a = MNetlist -> IO a

-- | A helper function to tell if a 'Net' is a netlist root
netIsRoot :: Net -> Bool
netIsRoot Net{netPrim=prim} = primIsRoot prim

-- | A helper function to tell if a 'Net' should not be optimised away
netDontKill :: Net -> Bool
netDontKill Net{netPrim=prim} = primDontKill prim

-- | A helper function to read a 'Net' from a 'MNetlist'
--readNet :: MNetlist -> InstId -> IO Net
readNet :: InstId -> MNetlistPass Net
readNet instId mnl = fromMaybe (error "encountered InstId with no matching Net")
                               <$> (readArray mnl instId)

-- | A helper type for 'Net' reference counting
type NetCounts = IOUArray InstId Int

-- | A 'Net' reference counting helper function
countNetRef :: MNetlistPass NetCounts
countNetRef mnl = do
  bounds <- getBounds mnl
  refCounts <- newArray bounds 0
  es <- getElems mnl
  -- count references for each Net
  let innerCount (InputWire (instId, _)) = do
        cnt <- readArray refCounts instId
        writeArray refCounts instId (cnt + 1)
      innerCount (InputTree _ inpts) = mapM_ innerCount inpts
  forM_ [e | Just e <- es] $ \net -> mapM_ innerCount (netInputs net)
  -- return reference counts
  return refCounts

-- | Repeat computation until a predicate holds
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM pred act =
  act >>= \x -> if pred x then return x else untilM pred act
