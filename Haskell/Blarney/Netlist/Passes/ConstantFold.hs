{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Netlist.Passes.ConstantFold
Description : A blarney netlist pass to fold constant values
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.ConstantFold (
  constantFold
) where

import Prelude
import qualified Data.Bits as B
import Control.Monad
import Data.IORef
import Data.Array.MArray

import Blarney.Netlist.Utils
import Blarney.Netlist.Passes.Utils

-- pattern helper to identify constant InputTree NetInputs
pattern Lit i <- InputTree (Const _ i) []
-- | Helper to evaluate constant Net
evalConstNet :: Net -> (Net, Bool)
-- Add
evalConstNet n@Net{ netPrim = Add w, netInputs = [Lit 0, x] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Add w, netInputs = [x, Lit 0] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Add w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 + a1), netInputs = [] }, True)
-- Sub -- TODO 0 - x could use a new negate primitive ?
evalConstNet n@Net{ netPrim = Sub w, netInputs = [x, Lit 0] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Sub w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 - a1), netInputs = [] }, True)
-- Mul
evalConstNet n@Net{ netPrim = Mul w, netInputs = [Lit 0, _] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mul w, netInputs = [_, Lit 0] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mul w, netInputs = [Lit 1, x] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Mul w, netInputs = [x, Lit 1] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Mul w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 * a1), netInputs = [] }, True)
-- Div
evalConstNet n@Net{ netPrim = Div w, netInputs = [Lit 0, _] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Div w, netInputs = [x, Lit 1] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Div w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `div` a1), netInputs = [] }, True)
-- Mod
evalConstNet n@Net{ netPrim = Mod w, netInputs = [Lit 0, _] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mod w, netInputs = [x, Lit 1] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mod w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `mod` a1), netInputs = [] }, True)
-- Not
evalConstNet n@Net{ netPrim = Not w, netInputs = [Lit a0] } =
  (n { netPrim = Const w ((2^w-1) `B.xor` a0), netInputs = [] }, True)
-- And
evalConstNet n@Net{ netPrim = And w, netInputs = [Lit 0, x] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = And w, netInputs = [x, Lit 0] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = And w, netInputs = [Lit v, x] } | v == 2^w-1 =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = And w, netInputs = [x, Lit v] } | v == 2^w-1 =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = And w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 B..&. a1), netInputs = [] }, True)
-- Or
evalConstNet n@Net{ netPrim = Or w, netInputs = [Lit 0, x] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Or w, netInputs = [x, Lit 0] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Or w, netInputs = [Lit v, x] } | v == 2^w-1 =
  (n { netPrim = Const w (2^w-1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Or w, netInputs = [x, Lit v] } | v == 2^w-1 =
  (n { netPrim = Const w (2^w-1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Or w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 B..|. a1), netInputs = [] }, True)
-- Xor
evalConstNet n@Net{ netPrim = Xor w, netInputs = [Lit 0, x] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Xor w, netInputs = [x, Lit 0] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Xor w, netInputs = [Lit v, x] } | v == 2^w-1 =
  (n { netPrim = Not w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Xor w, netInputs = [x, Lit v] } | v == 2^w-1 =
  (n { netPrim = Not w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = Xor w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `B.xor` a1), netInputs = [] }, True)
-- ShiftLeft
evalConstNet n@Net{ netPrim = ShiftLeft _ w, netInputs = [Lit 0, _] } =
  (n { netPrim = Const w 0, netInputs = [] }, True)
evalConstNet n@Net{ netPrim = ShiftLeft _ w, netInputs = [x, Lit 0] } =
  (n { netPrim = Identity w, netInputs = [x] }, True)
evalConstNet n@Net{ netPrim = ShiftLeft _ w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `B.shiftL` fromInteger (a1))
     , netInputs = [] }, True)
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
--ev Net{ netPrim = ShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
--ev Net{ netPrim = ArithShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
-- Comparisons
evalConstNet n@Net{ netPrim = Equal _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 == a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = NotEqual _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 /= a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = LessThan _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 < a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = LessThanEq _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 <= a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = ReplicateBit w, netInputs = [Lit a0] } =
  (n { netPrim = Const w (if a0 == 1 then 2^w-1 else 0), netInputs = [] }, True)
-- TODO XXX
--ev Net{ netPrim = ZeroExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
--ev Net{ netPrim = SignExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
-- TODO XXX
-- SelectBits
evalConstNet n@Net{ netPrim = SelectBits w hi lo, netInputs = [Lit a0] } =
  (n { netPrim   = Const (hi-lo+1) ((a0 `B.shiftR` lo) B..&. (2^(hi-lo+1)-1))
    , netInputs = [] }, True)
-- Concat
evalConstNet n@Net{ netPrim = Concat w0 w1, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const (w0+w1) ((a0 `B.shiftL` w1) B..|. a1)
     , netInputs = [] }, True)
-- Mux
evalConstNet n@Net{ netPrim = Mux _ w, netInputs = (Lit s):xs } =
  (n { netPrim = Identity w, netInputs = [xs !! fromInteger s] }, True)
-- Identity
evalConstNet n@Net{ netPrim = Identity w, netInputs = [Lit a0] } =
  (n { netPrim   = Const w a0, netInputs = [] }, True)
-- Fall-through case, no change
evalConstNet n = (n, False)

-- | Constant folding pass
constantFold :: MNetlistPass Bool
constantFold nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- Evaluate each constant 'Net' and update it in the 'Netlist'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    let (net', change) = evalConstNet net
    when change $ do writeIORef changed True -- keep track of changes
                     writeArray nl idx $ Just net' -- update 'Netlist'
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "foldConstant pass changed? " ++ show x
  readIORef changed -- return whether a change occured

