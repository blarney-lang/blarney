{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Blarney.Net
Description : Net primitive for Netlist construction
Copyright   : (c) Matthew Naylor, 2019
                  Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Net (
  Net(..)      -- Netlists are lists of Nets
, NetInput(..) -- Net inputs type
, WireId       -- Nets are connected by wires
, Expr(..)     -- Net inputs can also be an expression
, evalConstNet -- Net -> Net
) where

import Prelude
import qualified Data.Bits as B

import Blarney.BV
import Blarney.IfThenElse

-- |Netlists are lists of nets
data Net = Net { netPrim         :: Prim
               , netInstId       :: InstId
               , netInputs       :: [NetInput]
               , netOutputWidths :: [Width]
               , netName         :: Name
               } deriving Show

-- |A Net input can be: a wire, an int literal or a bit literal
data NetInput = InputWire WireId
              | InputExpr Expr
              deriving Show

-- |A wire is uniquely identified by an instance id and an output number
type WireId = (InstId, OutputNumber, Name)

-- |An expression used as a 'Net' input
data Expr = ConstE OutputWidth Integer
          | DontCareE OutputWidth
          deriving Show

-- Integer Constant Input Expr pattern helper
pattern Lit i <- InputExpr (ConstE _ i)
-- | Evaluate constant Net
evalConstNet :: Net -> Net
evalConstNet n@Net{ netPrim = Add w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 + a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Sub w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 - a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Mul w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 * a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Div w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 `div` a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Mod w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 `mod` a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Not w, netInputs = [Lit a0] } =
  n { netPrim = Const w ((2^w-1) `B.xor` a0), netInputs = [] }
evalConstNet n@Net{ netPrim = And w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 B..&. a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Or w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 B..|. a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Xor w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 `B.xor` a1), netInputs = [] }
evalConstNet n@Net{ netPrim = ShiftLeft w, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const w (a0 `B.shiftL` fromInteger (a1)), netInputs = [] }
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
--ev Net{ netPrim = ShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
--ev Net{ netPrim = ArithShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
evalConstNet n@Net{ netPrim = Equal _, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const 1 (if a0 == a1 then 1 else 0), netInputs = [] }
evalConstNet n@Net{ netPrim = NotEqual _, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const 1 (if a0 /= a1 then 1 else 0), netInputs = [] }
evalConstNet n@Net{ netPrim = LessThan _, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const 1 (if a0 < a1 then 1 else 0), netInputs = [] }
evalConstNet n@Net{ netPrim = LessThanEq _, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const 1 (if a0 <= a1 then 1 else 0), netInputs = [] }
evalConstNet n@Net{ netPrim = ReplicateBit w, netInputs = [Lit a0] } =
  n { netPrim = Const w (if a0 == 1 then 2^w-1 else 0), netInputs = [] }
-- TODO XXX
--ev Net{ netPrim = ZeroExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
--ev Net{ netPrim = SignExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
-- TODO XXX
evalConstNet n@Net{ netPrim = SelectBits w hi lo, netInputs = [Lit a0] } =
  n { netPrim   = Const (hi-lo+1) ((a0 `B.shiftR` lo) B..&. (2^(hi-lo+1)-1))
    , netInputs = [] }
evalConstNet n@Net{ netPrim = Concat w0 w1, netInputs = [Lit a0, Lit a1] } =
  n { netPrim = Const (w0+w1) ((a0 `B.shiftL` w1) B..|. a1), netInputs = [] }
evalConstNet n@Net{ netPrim = Mux w, netInputs = [Lit s, Lit a0, Lit a1] } =
  n { netPrim = Const w (if s == 0 then a1 else a0), netInputs = [] }
evalConstNet n@Net{ netPrim = CountOnes w, netInputs = [Lit a0] } =
  n { netPrim = Const w (toInteger (B.popCount a0)), netInputs = [] }
evalConstNet n@Net{ netPrim = Identity w, netInputs = [Lit a0] } =
  n { netPrim   = Const w a0, netInputs = [] }
evalConstNet n = n
