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
  Net(..)            -- Netlists are lists of Nets
, NetInput(..)       -- Net inputs type
, WireId             -- Nets are connected by wires
, Expr(..)           -- Net inputs can also be an expression
, evalConstNet       -- Turn a Net with const inputs into a Const Net
, netlistPasses      -- Toplevel function running the netlist passes
) where

import Prelude
import Data.Maybe
import Control.Monad
import Data.Array.IO
import Data.List (intercalate)
import qualified Data.Bits as B
import Data.Set (Set, empty, insert, toList, union)

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

-- | Finalise 'Name's in netlist to "v_"
finaliseNamesSimple :: IOArray InstId (Maybe Net)
                    -> IO (IOArray InstId (Maybe Net))
finaliseNamesSimple = mapArray inner
  where inner (Just net) = Just net { netInputs = map f (netInputs net)
                                    , netName   = Final "v"
                                    }
        inner Nothing = Nothing
        f (InputWire (i, n, _)) = InputWire (i, n, Final "v")
        f i = i

-- | Finalise names in Netlist preserving name hints
finaliseNames :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
finaliseNames arr = do
  bounds <- getBounds arr
  names :: IOArray InstId (Set String) <- newArray bounds empty
  pairs <- getAssocs arr
  -- accumulate names for each Net
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- update names with current netName
    updt names idx (netName net)
    -- update names with current net's input wires
    forM_ [wId | x@(InputWire wId) <- netInputs net] $ \(i, n, nm) -> do
      updt names i nm
  -- fold names back into Netlist
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- prepare new netInputs
    netInputs' <- forM (netInputs net) $ \inpt -> do
      case inpt of
        InputWire (i, n, _) -> do nm <- liftM genName (readArray names i)
                                  return $ InputWire (i, n, Final nm)
        x -> return x
    -- prepare new netName
    netName' <- liftM genName (readArray names idx)
    -- update the current net
    writeArray arr idx (Just net { netInputs = netInputs'
                                 , netName   = Final netName'
                                 })
  -- return mutated netlist
  return arr
  -- inner helpers
  where updt names i nm = do old <- readArray names i
                             case nm of
                               Hints nms -> writeArray names i (union nms old)
                               Final nm  -> writeArray names i (insert nm old)
        genName hints = if null hints then "v"
                        else intercalate "_" (toList hints)

-- | Constant folding pass
foldConstants :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
foldConstants = mapArray (fmap evalConstNet)

-- | Constant propagation pass
propagateConstants :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
propagateConstants arr = do
  pairs <- getAssocs arr
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- fold constant InputWire as InputExpr in current net inputs
    netInputs' <- forM (netInputs net) $ \inpt -> do
      case inpt of
        InputWire (instId, _, _) -> do
          inptNet <- fromMaybe (error "encountered InstId with no matching Net")
                           <$> (readArray arr instId)
          return $ case netPrim inptNet of
                     Const w i  -> InputExpr $ ConstE w i
                     DontCare w -> InputExpr $ DontCareE w
                     _          -> inpt
        _ -> return inpt
    -- update the current net
    writeArray arr idx (Just net { netInputs = netInputs' })
  -- return mutated netlist
  return arr

-- | Dead Net elimination pass
eliminateDeadNet :: IOArray InstId (Maybe Net)
                 -> IO (IOArray InstId (Maybe Net))
eliminateDeadNet arr = do
  bounds <- getBounds arr
  refCounts :: IOArray InstId Int <- newArray bounds 0
  pairs <- getAssocs arr
  -- count references for each Net
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    forM_ [wId | x@(InputWire wId) <- netInputs net] $ \(instId, _, _) -> do
      cnt <- readArray refCounts instId
      writeArray refCounts instId (cnt + 1)
  -- kill Nets with a null reference count
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    refCnt <- readArray refCounts idx
    if (refCnt == 0 && not (null $ netOutputWidths net))
      then writeArray arr idx Nothing
      else return ()
  -- return mutated netlist
  return arr

-- | Dead Net elimination pass
netlistPasses :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
netlistPasses arr = do
  tmpNetList <- finaliseNamesSimple arr
  --let constElim lst i = do tmp <- foldConstants lst
  --                         tmp <- propagateConstants tmp
  --                         return tmp
  --tmpNetList <- foldM constElim tmpNetList [0..100]
  --tmpNetList <- eliminateDeadNet tmpNetList
  return tmpNetList
