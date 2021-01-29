{- |
Module      : Blarney.Netlist.Passes.ConstantEliminate
Description : A blarney netlist pass to eliminate constants
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

'MNetlistPass' to eliminate constants, making use of the 'constantFold' and the
'constantPropagate' passes in a sequence until a fixed point is reached.

-}

module Blarney.Netlist.Passes.ConstantEliminate (
  constantEliminate
) where

import Prelude

import Blarney.Misc.MonadLoops
import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils
import Blarney.Netlist.Passes.ConstantFold
import Blarney.Netlist.Passes.ConstantPropagate

-- | Constant elimination pass
constantEliminate :: MNetlistPass s ()
constantEliminate mnlRef = constElim `untilPredM_` not
                           where constElim = do a <- constantFold mnlRef
                                                b <- constantPropagate mnlRef
                                                return $ a || b
