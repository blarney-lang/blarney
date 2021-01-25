{- |
Module      : Blarney.Netlist.Passes.ConstantPropage
Description : A blarney netlist pass to propagate constant values
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

'MNetlistPass' propagating constant 'Net's as 'InputTree' 'NetInput's to the
'Net's using them, and return whether such propagation occured during the pass.

-}

module Blarney.Netlist.Passes.ConstantPropagate (
  constantPropagate
) where

import Prelude
import Data.STRef
import Control.Monad
import Data.Array.MArray

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- | Constant propagation pass
constantPropagate :: MNetlistPass s Bool
constantPropagate mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  pairs <- getAssocs mnl -- list of nets with their index
  changed <- newSTRef False -- keep track of modifications to the 'Netlist'
  -- Turn constant 'InputWire' for each 'Net' into constant 'InputTree'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- process each 'NetInput' for the current 'Net'
    inputs' <- forM (netInputs net) $ \inpt -> do
      case inpt of
        InputWire (instId, _) -> do
          inptNet <- readNet instId mnlRef
          -- keep track of change when transforming into an 'InputTree'
          case netPrim inptNet of
            p@(Const _ _)  -> writeSTRef changed True >> return (InputTree p [])
            p@(DontCare _) -> writeSTRef changed True >> return (InputTree p [])
            _              -> return inpt
        _ -> return inpt
    -- update the current 'Net' in the 'Netlist'
    writeArray mnl idx (Just net { netInputs = inputs' })
  -- finish pass
  -- DEBUG HELP -- x <- readSTRef changed
  -- DEBUG HELP -- putStrLn $ "propagateConstant pass changed? " ++ show x
  readSTRef changed

