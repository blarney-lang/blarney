{-# LANGUAGE MultiWayIf         #-}
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
pattern LitDontCare <- InputTree (DontCare _) []
-- pattern helper to identify a 'Net' with specific 'Prim' and 'NetInput's
pattern NetP p is <- Net{ netPrim = p, netInputs = is }
-- | Helper to evaluate constant Net
evalConstNet :: Net -> (Net, Bool)
evalConstNet n@Net{..} = case n of
  -- special cases --
  -- Add
  NetP (Add w) [Lit 0, x] -> modNet (Identity w) [x]
  NetP (Add w) [x, Lit 0] -> modNet (Identity w) [x]
  -- Sub
  NetP (Sub w) [x, Lit 0] -> modNet (Identity w) [x]
  -- Mul
  NetP (Mul w sgn full) ins ->
    let ow = if full then 2*w else w
        extend = if | not  full -> Identity w
                    | not   sgn -> ZeroExtend w ow
                    | otherwise -> SignExtend w ow
    in case ins of
      [Lit 0, _] -> modNet (Const ow 0) []
      [_, Lit 0] -> modNet (Const ow 0) []
      [Lit 1, x] -> modNet extend [x]
      [x, Lit 1] -> modNet extend [x]
      -- Fall-through case, no change --
      _ -> (n, False)
  -- Div
  NetP (Div w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (Div w) [x, Lit 1] -> modNet (Identity w) [x]
  -- Mod
  NetP (Mod w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (Mod w) [_, Lit 1] -> modNet (Const w 0) []
  -- Not
  NetP (Not w) [Lit a0] -> modNet (Const w ((2^w-1) `B.xor` a0)) []
  -- And
  NetP (And w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (And w) [_, Lit 0] -> modNet (Const w 0) []
  NetP (And w) [Lit v, x] | v == 2^w-1 -> modNet (Identity w) [x]
  NetP (And w) [x, Lit v] | v == 2^w-1 -> modNet (Identity w) [x]
  -- Or
  NetP (Or w) [Lit 0, x] -> modNet (Identity w) [x]
  NetP (Or w) [x, Lit 0] -> modNet (Identity w) [x]
  NetP (Or w) [Lit v, x] | v == 2^w-1 -> modNet (Const w (2^w-1)) []
  NetP (Or w) [x, Lit v] | v == 2^w-1 -> modNet (Const w (2^w-1)) []
  -- Xor
  NetP (Xor w) [Lit 0, x] -> modNet (Identity w) [x]
  NetP (Xor w) [x, Lit 0] -> modNet (Identity w) [x]
  NetP (Xor w) [Lit v, x] | v == 2^w-1 -> modNet (Not w) [x]
  NetP (Xor w) [x, Lit v] | v == 2^w-1 -> modNet (Not w) [x]
  -- ShiftLeft
  NetP (ShiftLeft _ w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (ShiftLeft _ w) [x, Lit 0] -> modNet (Identity w) [x]
  -- ShiftRight
  NetP (ShiftRight _ w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (ShiftRight _ w) [x, Lit 0] -> modNet (Identity w) [x]
  -- ArithShiftRight
  NetP (ArithShiftRight _ w) [Lit 0, _] -> modNet (Const w 0) []
  NetP (ArithShiftRight _ w) [x, Lit 0] -> modNet (Identity w) [x]
  -- Mux
  NetP (Mux _ _ w) (Lit s:xs) -> modNet (Identity w) [xs !! fromInteger s]
  -- MergeWrites
  NetP (MergeWrites _ 0 w) [] -> modNet (DontCare w) []
  NetP (MergeWrites _ 1 w) [Lit 0, _] -> modNet (DontCare w) []
  NetP (MergeWrites _ 1 w) [Lit 1, x] -> modNet (Identity w) [x]
  -- Registers
  --NetP (RegisterEn Nothing w) [_, LitDontCare] -> modNet (DontCare w) []
  --NetP (RegisterEn Nothing w) [Lit 0, _] -> modNet (DontCare w) []
  --NetP (RegisterEn (Just i) w) [Lit 0, _] -> modNet (Const w i) []
  NetP (RegisterEn i w) [Lit 1, a] -> modNet (Register i w) [a]
  --NetP (Register Nothing w) [LitDontCare] -> modNet (DontCare w) []
  --NetP (Register (Just i) w) [Lit x] | i == x -> modNet (Const w i) []
  -- general unary op on literal --
  NetP _ [Lit a0] | Just f <- eval ->
    modNet (Const (primOutWidth netPrim Nothing) (head $ f [a0])) []
  -- general binary op on literals --
  NetP _ [Lit a0, Lit a1] | Just f <- eval ->
    modNet (Const (primOutWidth netPrim Nothing) (head $ f [a0, a1])) []
  -- Fall-through case, no change --
  _ -> (n, False)
  where eval = primSemEvalRaw netPrim
        modNet p is = (n { netPrim = p, netInputs = is }, True)

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
