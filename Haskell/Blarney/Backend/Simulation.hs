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

import Prelude
import Numeric
import Data.Char
import Data.Bits
import Data.Maybe
import Data.IORef
import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import System.Environment

import Blarney.Netlist
import Blarney.Misc.MonadLoops

err str = error $ "Blarney.Backend.Simulation: " ++ str

data SimEntry = SimUninitialized
              | SimValue Integer
              | SimReg Integer deriving (Show)
mkSimEntry :: Net -> IO SimEntry
mkSimEntry Net{ netPrim = (Const _ val) } = return $ SimValue val
mkSimEntry Net{ netPrim = (DontCare _) } = return $ SimValue 0 -- TODO other value?
mkSimEntry Net{ netPrim = (Register val _) } = return $ SimReg val
mkSimEntry Net{ netPrim = (RegisterEn val _) } = return $ SimReg val
mkSimEntry Net{ netPrim = TestPlusArgs plsArg } = do
  args <- getArgs
  return $ SimValue if '+' : plsArg `elem` args then 1 else 0
mkSimEntry _ = return SimUninitialized

data SimCtxt = SimCtxt { simNetlist        :: Netlist
                       , simEntriesCurrent :: IORef (IOArray InstId SimEntry)
                       , simEntriesNext    :: IORef (IOArray InstId SimEntry)
                       , simTerminate      :: IORef Bool }

mkSimCtxt :: Netlist -> IO SimCtxt
mkSimCtxt nl = do entries <- mapM mkSimEntry $ elems nl
                  currentArr <- newListArray (bounds nl) entries
                  currentRef <- newIORef currentArr
                  nextArr <- newListArray (bounds nl) entries
                  nextRef <- newIORef nextArr
                  terminateRef <- newIORef False
                  return $ SimCtxt nl currentRef nextRef terminateRef
readCurrent :: SimCtxt -> InstId -> IO SimEntry
readCurrent SimCtxt{..} instId = do
  entries <- readIORef simEntriesCurrent
  readArray entries instId
writeCurrent :: SimCtxt -> InstId -> SimEntry -> IO ()
writeCurrent SimCtxt{..} instId !entry = do
  entries <- readIORef simEntriesCurrent
  writeArray entries instId entry
writeNext :: SimCtxt -> InstId -> SimEntry -> IO ()
writeNext SimCtxt{..} instId !entry = do
  entries <- readIORef simEntriesNext
  writeArray entries instId entry
isOver :: SimCtxt -> IO Bool
isOver SimCtxt{..} = readIORef simTerminate
terminate :: SimCtxt -> IO ()
terminate SimCtxt{..} = writeIORef simTerminate True
stepCtxt :: SimCtxt -> IO ()
stepCtxt SimCtxt{..} = do current <- readIORef simEntriesCurrent
                          next <- readIORef simEntriesNext
                          writeIORef simEntriesCurrent next
                          writeIORef simEntriesNext current

simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  -- DBG
  --mapM_ print nl
  --putStrLn "============================"
  --
  ctxt <- mkSimCtxt nl
  let rootSteps = [ stepPrim n ctxt
                  | n@Net{ netPrim = p } <- elems nl, primIsRoot p ]
  sequence_ rootSteps >> stepCtxt ctxt `untilM_` isOver ctxt

-- step a primitive for each root
-- Inputs
-- Outputs
-- mutually recursive with evalInput
stepPrim :: Net -> SimCtxt -> IO ()
stepPrim net@Net{ netPrim = Display args, .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  doDisplay:inptArgs <- mapM (evalInput ctxt) netInputs
  when (doDisplay /= 0) do putStr . concat $ fmt args inptArgs
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show netInstId
  --
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
stepPrim net@Net{ netPrim = Finish, netInputs = [finish] } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show (netInstId net)
  --print net
  --
  doFinish <- evalInput ctxt finish
  when (doFinish /= 0) do terminate ctxt
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show (netInstId net)
  --
stepPrim net@Net{ netPrim = Assert msg, .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  [doAssert, pred] <- mapM (evalInput ctxt) netInputs
  when (doAssert /= 0 && pred == 0) do putStrLn msg >> terminate ctxt
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = Register _ _, netInputs = [inpt], .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  SimReg val <- readCurrent ctxt netInstId
  writeCurrent ctxt netInstId $ SimValue val
  inpt' <- evalInput ctxt inpt
  -- DBG
  --putStrLn $ "stepPrim Register new val " ++ show inpt
  --
  SimReg val <- readCurrent ctxt netInstId
  writeCurrent ctxt netInstId $ SimValue val
  writeNext ctxt netInstId $ SimReg inpt'
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = RegisterEn _ _, .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  SimReg val <- readCurrent ctxt netInstId
  writeCurrent ctxt netInstId $ SimValue val
  [en, inpt] <- mapM (evalInput ctxt) netInputs
  -- DBG
  --putStrLn $ "stepPrim RegisterEn, en " ++ show en ++ ", new val " ++ show inpt
  --
  when (en /= 0) do writeNext ctxt netInstId $ SimReg inpt
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = TestPlusArgs _ } ctxt = do
  -- DBG
  --print net
  --
  return ()
stepPrim net@Net{ .. } ctxt = do -- DBG
                                 --putStrLn $ "start stepPrim " ++ show netInstId
                                 --print net
                                 --
                                 ins <- mapM (evalInput ctxt) netInputs
                                 let val = evalPrim netPrim ins
                                 writeCurrent ctxt netInstId $ SimValue val
                                 writeNext ctxt netInstId SimUninitialized
                                 -- DBG
                                 --putStrLn $ "ending stepPrim " ++ show netInstId
                                 --

-- mutually recursive with stepPrim
evalInput :: SimCtxt -> NetInput -> IO Integer
evalInput ctxt (InputWire wId@(instId, _)) = do
  simEntry <- readCurrent ctxt instId
  -- DBG
  --print simEntry
  --
  case simEntry of
    SimUninitialized -> do let net = simNetlist ctxt ! instId
                           stepPrim net ctxt
                           evalInput ctxt (InputWire wId)
    SimReg val -> do let net = simNetlist ctxt ! instId
                     stepPrim net ctxt
                     return val
    SimValue val -> return val
evalInput ctxt (InputTree prim ins) = do ins' <- mapM (evalInput ctxt) ins
                                         return $ evalPrim prim ins'

withW :: Int -> Integer -> Integer
withW w i = (bit w - 1) .&. i

evalPrim :: Prim -> [Integer] -> Integer
evalPrim (Const w val) [] = withW w val
evalPrim (DontCare w) [] = 0 -- TODO some other value?
evalPrim (Add outW) [i0, i1] = withW outW $ i0 + i1
evalPrim (Sub outW) [i0, i1] = withW outW $ i0 - i1
evalPrim Mul { primMulInputWidth    = inW
             , primMulSigned        = sgn
             , primMulFullPrecision = precise }
             [i0, i1] = withW (2 * inW) (i0 * i1) -- TODO proper implementation
evalPrim (Div outW) [i0, i1] = withW outW $ i0 `div` i1
evalPrim (Mod outW) [i0, i1] = withW outW $ i0 `mod` i1
evalPrim (Not outW) [i0] = withW outW $ complement i0
evalPrim (And outW) [i0, i1] = withW outW $ i0 .&. i1
evalPrim (Or outW) [i0, i1] = withW outW $ i0 .|. i1
evalPrim (Xor outW) [i0, i1] = withW outW $ i0 `xor` i1
evalPrim (ShiftLeft inW outW) [i0, i1] = withW outW $ i0 `shiftL` fromInteger i1
evalPrim (ShiftRight inW outW) [i0, i1] = withW outW $ i0 `shiftR` fromInteger i1 -- TODO force usigned 'i0' (use 'Natural')
evalPrim (ArithShiftRight inW outW) [i0, i1] = withW outW $ i0 `shiftR` fromInteger i1
evalPrim (Equal inW) [i0, i1] = if i0 == i1 then 1 else 0
evalPrim (NotEqual inW) [i0, i1] = if i0 == i1 then 0 else 1
evalPrim (LessThan inW) [i0, i1] = if i0 < i1 then 1 else 0
evalPrim (LessThanEq inW) [i0, i1] = if i0 <= i1 then 1 else 0
evalPrim (ReplicateBit outW) [i0] = withW outW if i0 == 0 then zeroBits else complement zeroBits
evalPrim (ZeroExtend inW outW) [i0] = withW inW i0
evalPrim (SignExtend inW outW) [i0] = if sgn then sgnExt else withW outW i0
  where sgn = testBit i0 (inW - 1)
        sgnMask = (bit (outW - inW + 1) - 1) `shiftL` inW
        sgnExt = i0 .|. sgnMask
evalPrim (SelectBits inW hi lo) [i0] = withW (hi + 1) i0 `shiftR` lo
evalPrim (Concat i0W i1W) [i0, i1] = withW (i0W + i1W) $ i0 `shiftL` i1W + i1
evalPrim (Mux nIns outW) (sel:ins) = ins !! fromInteger sel
evalPrim (Identity outW) [i0] = i0
evalPrim prim _ = error $ "Blarney.Backend.Simulation: unsupported evalPrim primitive: " ++ show prim
