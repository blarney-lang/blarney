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

module Blarney.Net
  ( Net(..)          -- Netlists are lists of Nets
  , NetInput(..)     -- Net inputs type
  , WireId           -- Nets are connected by wires
  , Expr(..)         -- Net inputs can also be an expression
  , Flatten(..)      -- Monad for flattening a circuit (BV) to a netlist
  , doIO             -- Lift an IO computation to a Flatten computation
  , freshInstId      -- Obtain a fresh instance id
  , addNet           -- Add a net to the netlist
  , flatten          -- Flatten a bit vector to a netlist
  , evalConstNet     -- Net -> Net
  ) where

import Prelude
import Data.IORef
import Control.Monad
import qualified Data.Bits as B

import Blarney.BV
import Blarney.IfThenElse
-- For join lists
import qualified Blarney.JList as JL

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

-- |A wire is uniquely identified by an instance id and an output number
type WireId = (InstId, OutputNumber, Name)

-- |A reader/writer monad for accumulating the netlist
newtype Flatten a = Flatten { runFlatten :: FlattenR -> IO (FlattenW, a) }

-- |The reader component contains an IORef containing the next unique net id
type FlattenR = IORef Int

-- |The writer component contains the netlist and
-- an "undo" computation, which unperforms all IORef assignments.
type FlattenW = (JL.JList Net, IO ())

instance Monad Flatten where
  return a = Flatten (\r -> return ((JL.Zero, return ()), a))
  m >>= f  = Flatten (\r -> do ((w0, u0), a) <- runFlatten m r
                               ((w1, u1), b) <- runFlatten (f a) r
                               return ((w0 JL.:+: w1, u0 >> u1), b))

instance Applicative Flatten where
  pure = return
  (<*>) = ap

instance Functor Flatten where
  fmap = liftM

-- |Obtain a fresh 'InstId' with the next available id
freshInstId :: Flatten InstId
freshInstId = Flatten $ \r -> do
  id <- readIORef r
  writeIORef r (id+1)
  return ((JL.Zero, return ()), id)

-- |Add a net to the netlist
addNet :: Net -> Flatten ()
addNet net = Flatten $ \r -> return ((JL.One net, return ()), ())

-- |Add an "undo" computation
addUndo :: IO () -> Flatten ()
addUndo undo = Flatten $ \r -> return ((JL.Zero, undo), ())

-- |Lift an IO computation to a Flatten computation
doIO :: IO a -> Flatten a
doIO m = Flatten $ \r -> do
  a <- m
  return ((JL.Zero, return ()), a)

-- |Flatten bit vector to netlist
flatten :: BV -> Flatten NetInput
flatten BV{bvPrim=Const w v} = return $ InputExpr (ConstE w v)
flatten BV{bvPrim=DontCare w} = return $ InputExpr (DontCareE w)
flatten b@BV{bvName=name,bvInstRef=instRef} = do
  -- handle inst id traversal
  instIdVal <- doIO (readIORef instRef)
  case instIdVal of
    Nothing -> do
      id <- freshInstId
      doIO (writeIORef instRef (Just id))
      addUndo (writeIORef instRef Nothing)
      ins <- mapM flatten (bvInputs b)
      let net = Net { netPrim         = bvPrim b
                    , netInstId       = id
                    , netInputs       = ins
                    , netOutputWidths = (bvWidths b)
                    , netName         = name
                    }
      addNet net
      return $ InputWire (id, bvOutNum b, name)
    Just id -> return $ InputWire (id, bvOutNum b, name)
