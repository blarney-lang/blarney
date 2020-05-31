{-|
Module      : Blarney.Netlist.Passes.ConstantPropage
Description : A blarney netlist pass to propagate constant values
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.ConstantPropagate (
  constantPropagate
) where

import Prelude
import Control.Monad
import Data.IORef
import Data.Array.MArray

import Blarney.Netlist.Utils
import Blarney.Netlist.Passes.Utils

-- | Constant propagation pass
constantPropagate :: MNetlistPass Bool
constantPropagate nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- Turn constant 'InputWire' for each 'Net' into constant 'InputTree'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- process each 'NetInput' for the current 'Net'
    inputs' <- forM (netInputs net) $ \inpt -> do
      case inpt of
        InputWire (instId, _) -> do
          inptNet <- readNet nl instId
          -- keep track of change when transforming into an 'InputTree'
          case netPrim inptNet of
            p@(Const _ _)  -> writeIORef changed True >> return (InputTree p [])
            p@(DontCare _) -> writeIORef changed True >> return (InputTree p [])
            _              -> return inpt
        _ -> return inpt
    -- update the current 'Net' in the 'Netlist'
    writeArray nl idx (Just net { netInputs = inputs' })
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "propagateConstant pass changed? " ++ show x
  readIORef changed

