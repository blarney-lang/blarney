{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NoRebindableSyntax  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Blarney.Netlist.Passes.NamePropagate
Description : A blarney netlist pass to propagate names
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

'MNetlistPass' propagating through the netlist the name of the current
"destination" (register, bram, output...), augmenting individual 'Net's' name
hints.

-}

module Blarney.Netlist.Passes.NamePropagate (
  namePropagate
) where

import Prelude
import Data.STRef
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Set (insert, toList)

import Blarney.Netlist.Utils

-- | Name propagation pass
namePropagate :: forall s. MNetlistPass s ()
namePropagate ctxtRef = do
  mnl <- mnpNetlist <$> readSTRef ctxtRef -- expose the 'MNetlist'
  bounds <- getBounds mnl
  visited :: STUArray s InstId Bool <- newArray bounds False
  -- Push destination name down through netlist
  let visit destName instId = do
        isVisited <- readArray visited instId
        if not isVisited then do
          net@Net{ netPrim = prim
                 , netInputs = inpts
                 , netNameHints = hints
                 } <- readNet instId ctxtRef
          let inpts' = [instId | x@(InputWire (instId, _)) <- inpts]
          writeArray visited instId True
          -- Detect new destination and update destination name in recursive call
          if isDest prim then mapM_ (visit $ bestName net) inpts'
          else do
            let newHints = insert (NmSuffix 10 destName) hints
            writeArray mnl instId $ Just net{netNameHints = newHints}
            mapM_ (visit destName) inpts'
        else return ()
  --
  pairs <- getAssocs mnl -- list of nets with their index
  forM_ [i | x@(i, Just n) <- pairs, netIsRoot n] $ \instId -> do
    net <- readNet instId ctxtRef
    visit (bestName net) instId
  --
  where bestName Net{ netPrim = prim
                    , netNameHints = hints
                    , netInstId = instId
                    } = "DEST_" ++ nm
                        where nm = if null nms
                                   then primStr prim ++ "_id" ++ show instId
                                   else head nms
                              nms = [y | x@(NmRoot _ y) <- toList hints]
        --
        isDest Register{}     = True
        isDest RegisterEn{}   = True
        isDest BRAM{}         = True
        isDest Custom{}       = True
        isDest Input{}        = True
        isDest Output{}       = True
        isDest Display{}      = True
        isDest Finish         = True
        isDest TestPlusArgs{} = True
        isDest RegFileMake{}  = True
        isDest RegFileRead{}  = True
        isDest RegFileWrite{} = True
        isDest _ = False
