{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.Simulation
Description : 'Netlist' simulation
Copyright   : (c) Alexandre Joannou, 2021
License     : MIT
Stability   : experimental

Simulate a Blarney 'Netlist'.

-}

module Blarney.Backend.Simulation (
  simulateNetlist
) where

import Debug.Trace

import Prelude
import Numeric
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import Data.Char
import Data.Maybe
import System.Environment
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Array.IArray as IArray

import Blarney.Netlist
import Blarney.Misc.MonadLoops

-- debugging facilities
debugEnabled = False
dbg act = when debugEnabled act
err str = error $ "Blarney.Backend.Simulation: " ++ str
-- debugging facilities

-- | Simulate a 'Netlist' until a 'Finish' 'Prim' is triggered. This is
--   currently the "only" interface to the simulation module.
--   TODO: export the current SimulatorIfc after sanitizing it, and a bunch of
--         related helpers probably
simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  {- DBG -}
  dbg do mapM_ print nl
         putStrLn "============================"
         --putStrLn $ "length simEffects = " ++ show (length simEffects)
         --print simTerminate
  {- DBG -}
  -- get command line arguments
  args <- getArgs
  -- simulator compiled from the input netlist
  let SimulatorIfc{..} = compile nl args Map.empty
  -- here, while the termination stream does not indicate the end of the
  -- simulation, we apply all effects from the current simulation cycle and
  -- also print TICK
  mapM_ (\(effects, end) -> sequence_ effects {->> print ("TICK: " ++ show end)-})
        (zip (List.transpose simEffects) (takeWhileInclusive not simTerminate))
  where takeWhileInclusive _ [] = []
        takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                                 else []

--------------------------------------------------------------------------------
-- helper types

-- | A type representing the value of a 'Signal' on a given cycle
type Sample = Integer

-- | A type representing a signal
type Signal = [Sample]

-- | A type representing s stream of simulation 'IO's to supplement output
--   'Signal's
type SimEffect = [IO ()]

-- | Array to memoize 'Signal' during compilation
type MemoSignals s = STArray s InstId [Maybe Signal]

-- | Context for compilation
data CompCtxt s = CompCtxt { compMemo :: MemoSignals s
                           , compArgs :: [String]
                           , compNl   :: Netlist }

-- | create a 'CompCtxt' from a 'Netlist'
mkCompCtxt :: Netlist -> [String] -> ST s (CompCtxt s)
mkCompCtxt nl args = do
  -- signal memoization table
  memo <- newListArray (IArray.bounds nl)
                       [ replicate (length $ primOutputs p) Nothing
                       | Net { netPrim = p } <- IArray.elems nl ]
  return $ CompCtxt { compMemo = memo, compArgs = args, compNl = nl }

-- | compile helper functions
type CompFun s a b = CompCtxt s -> a -> ST s b

-- | a dictionary of named 'Signal's
type SignalMap = Map.Map String Signal

-- | A 'Simulator''s interface
data SimulatorIfc = SimulatorIfc {
    -- | set of streams of effects
    simEffects   :: [SimEffect]
    -- | stream of booleans indicating termination
  , simTerminate :: [Bool]
    -- | dictionary of output signals
  , simOutputs   :: SignalMap }

-- | A type defining the interface to a simulator
type Simulator = SignalMap -> SimulatorIfc

--------------------------------------------------------------------------------
-- netlist compilation utilities

-- | A function to turn a 'Netlist' into a 'Simulator'
compile :: Netlist -> [String] -> Simulator
compile nl args inputStreams = runST do
  -- prepare compilation context
  ctxt <- mkCompCtxt nl args
  -- compile simulation effects streams
  effectStreams <- sequence [ compileSimEffect ctxt n
                            | n@Net{ netPrim = p } <- IArray.elems nl
                            , case p of Display _ -> True
                                        _         -> False ]
  -- compile simulation termination stream
  endStreams <- sequence [ compileTermination ctxt n
                         | n@Net{ netPrim = p } <- IArray.elems nl
                         , case p of Finish   -> True
                                     Assert _ -> True
                                     _        -> False ]
  -- compile simulation outputs streams
  let outputs = [ (nm, compileOutputSignal ctxt o)
                | o@Net{ netPrim = Output _ nm } <- IArray.elems nl ]
  let (nms, sigsST) = unzip outputs
  sigs <- sequence sigsST
  let outputStreams = zip nms sigs
  -- wrap compilation result into a SimulatorIfc
  return $ SimulatorIfc { simEffects   = effectStreams
                        , simTerminate = map or (List.transpose endStreams)
                        , simOutputs   = Map.fromList outputStreams }

-- | compile the 'SimEffect' stream of simulation effects for a given simulation
--   effect capable 'Net' (currently, only 'Display' nets)
compileSimEffect :: CompFun s Net SimEffect
compileSimEffect ctxt Net{ netPrim = Display args, .. } = do
  netInputs' <- mapM (compileNetInputSignal ctxt) netInputs
  return $ map (\(en:inpts) -> when (en /= 0) do putStr . concat $ fmt args inpts)
               (List.transpose netInputs')
  where fmt [] _ = []
        fmt (DisplayArgString s : rest) ins = escape s : fmt rest ins
        fmt (DisplayArgBit _ r p z : rest) (x:ins) =
          (pad (fromMaybe 0 p) z $ radixShow r x) : fmt rest ins
        escape str = concat [if c == '%' then "%%" else [c] | c <- str]
        radixShow Bin n = showIntAtBase  2 intToDigit n ""
        radixShow Dec n = showIntAtBase 10 intToDigit n ""
        radixShow Hex n = showIntAtBase 16 intToDigit n ""
        pad n zero str =
          replicate (n - length str) (if zero then '0' else ' ') ++ str

-- | compile the termination stream as a '[Bool]' for a given termination
--   capable 'Net' (currently only 'Finish' nets)
compileTermination :: CompFun s Net [Bool]
compileTermination ctxt Net{ netPrim = Finish, netInputs = [en] } = do
  en' <- compileNetInputSignal ctxt en
  return $ (/= 0) <$> en'

-- | compile the output 'Signal' for a given output 'Net'
--   (currently not supported)
compileOutputSignal :: CompFun s Net Signal
compileOutputSignal ctxt n = do
  return $ repeat 0 -- TODO

-- | compile the 'Signal' associated with a 'WireId' (memoized)
compileWireIdSignal :: CompFun s WireId Signal
compileWireIdSignal ctxt@CompCtxt{..} (instId, outNm) = do
  thunks <- readArray compMemo instId
  let net = getNet compNl instId
  case primOutIndex (netPrim net) outNm of
    Just idx ->
      case thunks !! idx of
        Just signal -> return signal
        Nothing -> mdo
          -- first write the memoization array
          let thunks' = take idx thunks ++ Just signal : drop (idx+1) thunks
          writeArray compMemo instId thunks'
          -- then compile the signal
          {- DBG -}
          dbg $ traceM $ "compileWireIdSignal " ++ show (instId, outNm) ++ " >>>> (call) compileNetIndexedOuptutSignal " ++ show (net, idx)
          {- DBG -}
          signal <- compileNetIndexedOuptutSignal ctxt (net, idx)
          {- DBG -}
          dbg $ traceM $ "compileNetIndexedOuptutSignal " ++ show (net, idx) ++ " >>>> (return) compileWireIdSignal " ++ show (instId, outNm)
          {- DBG -}
          return signal
    Nothing -> err $ "unknown output " ++ show outNm ++ " for net " ++ show net

-- | compile the 'Signal' associated with an indexed output of a given 'Net'
--   (not memoized)
compileNetIndexedOuptutSignal :: CompFun s (Net, Int) Signal
compileNetIndexedOuptutSignal ctxt (Net{..}, idx) = do
  {- DBG -}
  dbg $ traceM $ "compileNetIndexedOuptutSignal " ++ show (netPrim, netInstId, idx) ++ " >>>> (call) map compileNetInputSignal " ++ show netInputs
  {- DBG -}
  ins <- mapM (compileNetInputSignal ctxt) netInputs
  {- DBG -}
  dbg $ traceM $ "map compileNetInputSignal " ++ show netInputs ++ " >>>> (return) compileNetIndexedOuptutSignal " ++ show (netPrim, netInstId, idx)
  {- DBG -}
  primOuts <- compilePrim ctxt (netPrim, ins)
  return $ primOuts !! idx

-- | compile the 'Signal' associated with a 'NetInput' (not memoized)
compileNetInputSignal :: CompFun s NetInput Signal
compileNetInputSignal ctxt (InputWire wId) = do
  {- DBG -}
  dbg $ traceM $ "compileNetInputSignal InputWire " ++ show wId ++ " >>>> (call) compileWireIdSignal " ++ show wId
  {- DBG -}
  signal <- compileWireIdSignal ctxt wId
  {- DBG -}
  dbg $ traceM $ "compileWireIdSignal " ++ show wId ++ " >>>> (return) compileNetInputSignal InputWire " ++ show wId
  {- DBG -}
  return signal
compileNetInputSignal ctxt (InputTree prim ins) = do
  {- DBG -}
  dbg $ traceM $ "compileNetInputSignal InputTree " ++ show (prim, ins) ++ " >>>> (call) map compileNetInputSignal " ++ show ins
  {- DBG -}
  ins' <- mapM (compileNetInputSignal ctxt) ins
  {- DBG -}
  dbg $ traceM $ "map compileNetInputSignal " ++ show ins ++ " >>>> (return) compileNetInputSignal InputTree " ++ show (prim, ins)
  {- DBG -}
  primOuts <- compilePrim ctxt (prim, ins')
  return $ primOuts !! 0 -- TODO check that we only eval single output primitives





-- | Describe how a primitive, given some input 'Signal's produces its output
--   'Signals' (most often only the one output 'Signal' returned as a singleton
--   list)
--   Note: the CompCtxt argument is needed for primitives such as TestPlusArgs
compilePrim :: CompFun s (Prim, [Signal]) [Signal]
compilePrim CompCtxt{..} (TestPlusArgs plsArg, []) = return [repeat isPresent]
  where isPresent = if '+' : plsArg `elem` compArgs then 1 else 0
compilePrim _ (Const w val, []) = return [repeat $ clamp w $ fromInteger val]
compilePrim _ (DontCare w,  []) = return [repeat 0] -- TODO some other value?
compilePrim _ (Add w, [i0, i1]) = return [zipWith (\x y -> clamp w (x + y)) i0 i1]
compilePrim _ (Sub w, [i0, i1]) = return [zipWith (\x y -> clamp w (x - y)) i0 i1]
compilePrim _ ( Mul { primMulInputWidth    = w
                    , primMulSigned        = sgn
                    , primMulFullPrecision = precise }
              , [i0, i1] ) = -- TODO proper implementation
  return [zipWith (\x y -> clamp (2*w) (x * y)) i0 i1]
compilePrim _ (Div w, [i0, i1]) = return [zipWith (\x y -> clamp w (x `div` y)) i0 i1]
compilePrim _ (Mod w, [i0, i1]) = return [zipWith (\x y -> clamp w (x `mod` y)) i0 i1]
compilePrim _ (Not w, [i0]) = return [map (clamp w . complement) i0]
compilePrim _ (And w, [i0, i1]) = return [zipWith (\x y -> clamp w (x .&. y)) i0 i1]
compilePrim _ (Or w, [i0, i1]) = return [zipWith (\x y -> clamp w (x .|. y)) i0 i1]
compilePrim _ (Xor w, [i0, i1]) = return [zipWith (\x y -> clamp w (x `xor` y)) i0 i1]
compilePrim _ (ShiftLeft _ w,       [i0, i1]) = return [zipWith (\x y -> clamp w $ x `shiftL` fromIntegral y) i0 i1]
compilePrim _ (ShiftRight _ w,      [i0, i1]) = return [zipWith (\x y -> clamp w $ x `shiftR` fromIntegral y) i0 i1] -- TODO force usigned 'i0' (use 'Natural')
compilePrim _ (ArithShiftRight _ w, [i0, i1]) = return [zipWith (\x y -> clamp w $ x `shiftR` fromIntegral y) i0 i1]
compilePrim _ (Equal _,      [i0, i1]) = return [zipWith (\x y -> if x == y then 1 else 0) i0 i1]
compilePrim _ (NotEqual _,   [i0, i1]) = return [zipWith (\x y -> if x /= y then 1 else 0) i0 i1]
compilePrim _ (LessThan _,   [i0, i1]) = return [zipWith (\x y -> if x  < y then 1 else 0) i0 i1]
compilePrim _ (LessThanEq _, [i0, i1]) = return [zipWith (\x y -> if x <= y then 1 else 0) i0 i1]
compilePrim _ (ReplicateBit w, [i0]) = return [map (\x -> clamp w if x == 0 then zeroBits else complement zeroBits) i0]
compilePrim _ (ZeroExtend w _, [i0]) = return [map (clamp w) i0]
compilePrim _ (SignExtend inW w, [i0]) =
  return [map (\x -> let sgn = testBit x (inW - 1)
                         sgnMask = (bit (w - inW + 1) - 1) `shiftL` inW
                         sgnExt = x .|. sgnMask
                     in if sgn then sgnExt else clamp w x) i0]
compilePrim _ (SelectBits _ hi lo, [i0]) = return [map (\x -> clamp (hi + 1) x `shiftR` lo) i0]
compilePrim _ (Concat w0 w1, [i0, i1]) = return [zipWith (\x y -> clamp (w0 + w1) $ x `shiftL` w1 + y) i0 i1]
compilePrim _ (Identity _, [i0]) = return [i0]
compilePrim _ (Mux _ w, [ss, i0, i1]) =
  return [zipWith3 (\s x y -> if s == 0 then x else y) ss i0 i1]
compilePrim _ (RegisterEn i w, [ens, inpts]) = return [scanl f i (zip ens inpts)]
  where f prev (en, inpt) = if en /= 0 then inpt else prev
compilePrim _ (prim, ins) =
  return $ List.transpose $ primSemEval prim <$> (List.transpose ins)
--compilePrim _ (prim, ins) = return $ transpose $ evalPrim prim <$> (transpose ins)
--compilePrim _ (prim, ins) = return $ err $ "can't compilePrim " ++ show prim ++ ", " ++ show ins


-- XXX unclear if needed XXX
---- | Lazy transpose, assumes all rows have same length
--transpose [] = [[]]
--transpose [xs] = [[x] | x <- xs]
----transpose (xs : xss) = lzw (:) xs (transpose xss)
--transpose (xs : xss) = zipWith (:) xs (transpose xss)
--
---- | Left zipWith, assumes lists are same size
--lzw op [] ys = []
--lzw op (x:xs) ys = (x `op` head ys) : lzw op xs (tail ys)
