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
type Signal = Integer

mkSimEntry :: Net -> IO (Maybe Signal)
mkSimEntry Net{ netPrim = (Const _ val) }       = return $ Just val
mkSimEntry Net{ netPrim = (DontCare _) }        = return $ Just 0 -- TODO other value?
mkSimEntry Net{ netPrim = (Register val _) }    = return $ Just val
mkSimEntry Net{ netPrim = (RegisterEn val _) }  = return $ Just val
mkSimEntry Net{ netPrim = TestPlusArgs plsArg } = do
  args <- getArgs
  return $ Just if '+' : plsArg `elem` args then 1 else 0
mkSimEntry _ = return Nothing

data SimCtxt = SimCtxt { simNetlist        :: Netlist
                       , simEntriesReset   :: Array InstId (Maybe Signal)
                       , simEntriesCurrent :: IOArray InstId (Maybe Signal)
                       , simEntriesNext    :: IOArray InstId (Maybe Signal)
                       , simTerminate      :: IORef Bool }

mkSimCtxt :: Netlist -> IO SimCtxt
mkSimCtxt nl = do resetEntries <- mapM mkSimEntry $ elems nl
                  let resetArr = listArray (bounds nl) resetEntries
                  currentArr <- thaw resetArr
                  nextArr <- thaw resetArr
                  terminateRef <- newIORef False
                  return $ SimCtxt nl resetArr currentArr nextArr terminateRef
readCurrent :: SimCtxt -> InstId -> IO Signal
readCurrent ctxt@SimCtxt{..} instId = do
  msig <- readArray simEntriesCurrent instId
  case msig of Just sig -> return sig
               Nothing  -> do evalCurrentSignal ctxt instId
                              readCurrent ctxt instId
writeCurrent :: SimCtxt -> InstId -> Signal -> IO ()
writeCurrent SimCtxt{..} instId !sig = do
  writeArray simEntriesCurrent instId $ Just sig
writeNext :: SimCtxt -> InstId -> Maybe Signal -> IO ()
writeNext SimCtxt{..} instId !msig = do
  writeArray simEntriesNext instId msig
cleanCurrent :: SimCtxt -> IO ()
cleanCurrent SimCtxt{..} = do
  simEntriesCurrent :: IOArray InstId (Maybe Signal) <- thaw simEntriesReset
  return ()

primIsSimRoot :: Prim -> Bool
primIsSimRoot (RegisterEn _ _) = True
primIsSimRoot (Register   _ _) = True
primIsSimRoot p = primIsRoot p

simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  -- DBG
  --mapM_ print nl
  --putStrLn "============================"
  --
  let rootSteps c = [ stepPrim n c
                    | n@Net{ netPrim = p } <- elems nl, primIsSimRoot p ]
  ctxt@SimCtxt{..} <- mkSimCtxt nl
  let evenCtxt = ctxt
  let oddCtxt  = ctxt{ simEntriesCurrent = simEntriesNext
                     , simEntriesNext    = simEntriesCurrent }
  let stepEven  = sequence_ $ rootSteps evenCtxt
  let stepOdd   = sequence_ $ rootSteps oddCtxt
  let cleanEven = cleanCurrent evenCtxt
  let cleanOdd  = cleanCurrent oddCtxt
  let loop isEven = do if isEven then stepEven >> cleanEven
                                 else stepOdd  >> cleanOdd
                       done <- readIORef simTerminate
                       when (not done) do loop $ not isEven
  loop True

evalCurrentSignal :: SimCtxt -> InstId -> IO ()
evalCurrentSignal ctxt instId = stepPrim (simNetlist ctxt ! instId) ctxt

-- step a primitive for each root
-- Inputs
-- Outputs
-- mutually recursive with readInput
stepPrim :: Net -> SimCtxt -> IO ()
stepPrim net@Net{ netPrim = Display args, netInputs = en:inpts } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do inpts' <- mapM (readInput ctxt) inpts
                     putStr . concat $ fmt args inpts'
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
stepPrim net@Net{ netPrim = Finish, netInputs = [en] } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show (netInstId net)
  --print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do writeIORef (simTerminate ctxt) True
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show (netInstId net)
  --
stepPrim net@Net{ netPrim = Assert msg, netInputs = [en, pred] } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do pred' <- readInput ctxt pred
                     when (pred' == 0) do putStrLn msg
                                          writeIORef (simTerminate ctxt) True
  -- DBG
  --putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = Register _ _, netInputs = [inpt], .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  inpt' <- readInput ctxt inpt
  writeNext ctxt netInstId $ Just inpt'
stepPrim net@Net{ netPrim = RegisterEn _ _
                , netInputs = [en, inpt]
                , .. } ctxt = do
  -- DBG
  --putStrLn $ "start stepPrim " ++ show netInstId
  --print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do inpt' <- readInput ctxt inpt
                     writeNext ctxt netInstId $ Just inpt'
stepPrim net@Net{ netPrim = TestPlusArgs _ } ctxt = return ()
stepPrim net@Net{..} ctxt = do -- DBG
                               --putStrLn $ "start stepPrim " ++ show netInstId
                               --print net
                               --
                               ins <- mapM (readInput ctxt) netInputs
                               let val = evalPrim netPrim ins
                               writeCurrent ctxt netInstId val
                               writeNext ctxt netInstId Nothing
                               -- DBG
                               --putStrLn $ "ending stepPrim " ++ show netInstId
                               --

-- mutually recursive with stepPrim
readInput :: SimCtxt -> NetInput -> IO Signal
readInput ctxt (InputWire wId@(instId, _)) = readCurrent ctxt instId
readInput ctxt (InputTree prim ins) = do ins' <- mapM (readInput ctxt) ins
                                         return $ evalPrim prim ins'

withW :: Int -> Signal -> Signal
withW w i = (bit w - 1) .&. i

evalPrim :: Prim -> [Signal] -> Signal
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
evalPrim (Or outW)  [i0, i1] = withW outW $ i0 .|. i1
evalPrim (Xor outW) [i0, i1] = withW outW $ i0 `xor` i1
evalPrim (ShiftLeft inW outW)       [i0, i1] = withW outW $ i0 `shiftL` fromInteger i1
evalPrim (ShiftRight inW outW)      [i0, i1] = withW outW $ i0 `shiftR` fromInteger i1 -- TODO force usigned 'i0' (use 'Natural')
evalPrim (ArithShiftRight inW outW) [i0, i1] = withW outW $ i0 `shiftR` fromInteger i1
evalPrim (Equal inW)      [i0, i1] = if i0 == i1 then 1 else 0
evalPrim (NotEqual inW)   [i0, i1] = if i0 /= i1 then 1 else 0
evalPrim (LessThan inW)   [i0, i1] = if i0  < i1 then 1 else 0
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
