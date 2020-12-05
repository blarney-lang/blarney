{-|
Module      : Blarney.Netlist.Passes.ConstantEliminate
Description : A blarney netlist pass to eliminate constants
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.ConstantEliminate (
  constantEliminate
) where

import Prelude

import Blarney.Netlist.Passes.Utils
import Blarney.Netlist.Passes.ConstantFold
import Blarney.Netlist.Passes.ConstantPropagate

-- | A constant elimination pass
constantEliminate :: MNetlistPass s ()
constantEliminate mnl = untilM not constElim >> return ()
                        where constElim = do a <- constantFold mnl
                                             b <- constantPropagate mnl
                                             return $ a || b
