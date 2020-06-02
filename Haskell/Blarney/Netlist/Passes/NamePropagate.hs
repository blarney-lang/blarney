{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Netlist.Passes.NamePropagate
Description : A blarney netlist pass to propagate names
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.NamePropagate (
  namePropagate
) where

import Prelude
import Data.Array.IO
import Control.Monad
import Data.Set (insert, toList)

import Blarney.Netlist.Passes.Utils

-- | propagate names through the Netlist
namePropagate :: MNetlist -> IO ()
namePropagate mnl = do
  bounds <- getBounds mnl
  visited :: IOUArray InstId Bool <- newArray bounds False
  -- Push destination name down through netlist
  let visit destName instId = do
      isVisited <- readArray visited instId
      if not isVisited then do
        net@Net{ netPrim = prim
               , netInputs = inpts
               , netNameHints = hints
               } <- readNet instId mnl
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
    net <- readNet instId mnl
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
        isDest TrueDualBRAM{} = True
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
