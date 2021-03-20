{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{- |
Module      : Blarney.Netlist.Passes.ConstantFold
Description : A blarney netlist pass to fold constant values
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

'MNetlistPass' simplyfying 'Net's with constant inputs and returns whether any
such change occured during the pass.

-}

module Blarney.Netlist.Passes.ConstantFold (
  constantFold
) where

import Prelude
import Data.STRef
import Control.Monad
import Data.Array.MArray
import qualified Data.Bits as B

import Blarney.Netlist.Passes.Types
import Blarney.Netlist.Passes.Utils

-- pattern helper to identify constant InputTree NetInputs
pattern Lit i <- InputTree (Const _ i) []
-- | Helper to evaluate constant Net
evalConstNet :: Net -> (Net, Bool)
evalConstNet n@Net{..} = case n of
  -- special cases --
  -- Add
  Net{ netPrim = Add w, netInputs = [Lit 0, x] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Add w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Sub
  Net{ netPrim = Sub w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Mul
  Net{ netPrim = Mul w _ _, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = Mul w _ _, netInputs = [_, Lit 0] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = Mul w _ _, netInputs = [Lit 1, x] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Mul w _ _, netInputs = [x, Lit 1] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Div
  Net{ netPrim = Div w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = Div w, netInputs = [x, Lit 1] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Mod
  Net{ netPrim = Mod w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = Mod w, netInputs = [_, Lit 1] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  -- Not
  Net{ netPrim = Not w, netInputs = [Lit a0] } ->
    (n { netPrim = Const w ((2^w-1) `B.xor` a0), netInputs = [] }, True)
  -- And
  Net{ netPrim = And w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = And w, netInputs = [_, Lit 0] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = And w, netInputs = [Lit v, x] } | v == 2^w-1 ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = And w, netInputs = [x, Lit v] } | v == 2^w-1 ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Or
  Net{ netPrim = Or w, netInputs = [Lit 0, x] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Or w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Or w, netInputs = [Lit v, x] } | v == 2^w-1 ->
    (n { netPrim = Const w (2^w-1), netInputs = [] }, True)
  Net{ netPrim = Or w, netInputs = [x, Lit v] } | v == 2^w-1 ->
    (n { netPrim = Const w (2^w-1), netInputs = [] }, True)
  -- Xor
  Net{ netPrim = Xor w, netInputs = [Lit 0, x] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Xor w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  Net{ netPrim = Xor w, netInputs = [Lit v, x] } | v == 2^w-1 ->
    (n { netPrim = Not w, netInputs = [x] }, True)
  Net{ netPrim = Xor w, netInputs = [x, Lit v] } | v == 2^w-1 ->
    (n { netPrim = Not w, netInputs = [x] }, True)
  -- ShiftLeft
  Net{ netPrim = ShiftLeft _ w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = ShiftLeft _ w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- ShiftRight
  Net{ netPrim = ShiftRight _ w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = ShiftRight _ w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- ArithShiftRight
  Net{ netPrim = ArithShiftRight _ w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w 0, netInputs = [] }, True)
  Net{ netPrim = ArithShiftRight _ w, netInputs = [x, Lit 0] } ->
    (n { netPrim = Identity w, netInputs = [x] }, True)
  -- Mux
  Net{ netPrim = Mux _ w, netInputs = (Lit s):xs } ->
    (n { netPrim = Identity w, netInputs = [xs !! fromInteger s] }, True)
  -- Registers
  Net{ netPrim = RegisterEn i w, netInputs = [Lit 0, _] } ->
    (n { netPrim = Const w i, netInputs = [] }, True)
  Net{ netPrim = RegisterEn i w, netInputs = [Lit 1, a] } ->
    (n { netPrim = Register i w, netInputs = [a] }, True)
  Net{ netPrim = Register i w, netInputs = [Lit x] } | i == x ->
    (n { netPrim = Const w i, netInputs = [] }, True)
  -- Avoid matching Register in general fall through
  Net{ netPrim = Register i w, netInputs = [a] } -> (n, False)
  -- general unary op on literal --
  Net{ netInputs = [Lit a0] } ->
    ( n { netPrim = Const (primOutWidth netPrim Nothing) (head $ eval [a0])
        , netInputs = [] }
    , True )
  -- general binary op on literals --
  Net{ netInputs = [Lit a0, Lit a1] } ->
    ( n { netPrim = Const (primOutWidth netPrim Nothing) (head $ eval [a0, a1])
        , netInputs = [] }
    , True )
  -- Fall-through case, no change --
  _ -> (n, False)
  where eval = primSemEval netPrim

-- | Constant folding pass
constantFold :: MNetlistPass s Bool
constantFold mnlRef = do
  mnl <- readSTRef mnlRef -- expose the 'MNetlist'
  pairs <- getAssocs mnl -- list of nets with their index
  changed <- newSTRef False -- keep track of modifications to the 'Netlist'
  -- Evaluate each constant 'Net' and update it in the 'Netlist'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    let (net', change) = evalConstNet net
    when change $ do writeSTRef changed True -- keep track of changes
                     writeArray mnl idx $ Just net' -- update 'Netlist'
  -- finish pass
  -- DEBUG HELP -- x <- readSTRef changed
  -- DEBUG HELP -- putStrLn $ "foldConstant pass changed? " ++ show x
  readSTRef changed -- return whether a change occured
