{-|
Module      : Blarney.Netlist.Passes.DeadNetEliminate
Description : A blarney netlist pass to eliminate Nets that are not referenced
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.DeadNetEliminate (
  deadNetEliminate
) where

import Prelude
import Data.IORef
import Control.Monad
import Data.Array.MArray

import Blarney.Netlist.Passes.Utils

-- | Dead Net elimination pass
deadNetEliminate :: MNetlist -> IO Bool
deadNetEliminate nl = do
  refCounts <- countNetRef nl -- reference count for each 'Net'
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- kill Nets with a null reference count
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    refCnt <- readArray refCounts idx
    when (refCnt == 0 && (not . netIsRoot) net && (not . netDontKill) net) $ do
      writeArray nl idx Nothing
      writeIORef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "deadNetEliminate pass changed? " ++ show x
  readIORef changed
  where alsoDontKill Net{netPrim=Output _ _} = True
        alsoDontKill Net{netPrim=RegFileWrite _} = True
        alsoDontKill _ = False

