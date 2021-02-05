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

debugEnabled = False
dbg act = when debugEnabled act
err str = error $ "Blarney.Backend.Simulation: " ++ str
type Signal = Integer

data SignalThunk = Value | ValueBRAM Signal (IOArray Signal Signal)
data SignalHandle = Unevaluated SignalThunk | Evaluated Signal
mkSignalHandle :: Net -> IO [SignalHandle]
mkSignalHandle Net{ netPrim = (Const _ val) }      = return [Evaluated val]
mkSignalHandle Net{ netPrim = (DontCare _) }       = return [Evaluated 0] -- TODO other value?
mkSignalHandle Net{ netPrim = (Register val _) }   = return [Evaluated val]
mkSignalHandle Net{ netPrim = (RegisterEn val _) } = return [Evaluated val]
mkSignalHandle Net{ netPrim = TestPlusArgs plsArg } = do
  args <- getArgs
  return [Evaluated if '+' : plsArg `elem` args then 1 else 0]
mkSignalHandle Net{ netPrim = BRAM{..} } = do
  firstBytes <- case ramInitFile of
                  Just f -> do initContent <- lines <$> readFile f
                               return $ fst . head . readHex <$> initContent
                  _ -> return []
  dataArray <- newListArray (0, 2^ramAddrWidth-1)
                            (firstBytes ++ repeat errUninit)
  fstVal <- readArray dataArray 0
  let sigHandle = Unevaluated $ ValueBRAM fstVal dataArray
  return case ramKind of BRAMTrueDualPort -> [sigHandle, sigHandle]
                         _ -> [sigHandle]
  --where errUninit = err "accessing uninitialized memory"
  where errUninit = 0
mkSignalHandle _ = return [Unevaluated Value]

data SimCtxt = SimCtxt { simNetlist        :: Netlist
                       , simResetHandles   :: Array   InstId [SignalHandle]
                       , simCurrentHandles :: IOArray InstId [SignalHandle]
                       , simNextHandles    :: IOArray InstId [SignalHandle]
                       , simTerminate      :: IORef Bool }

mkSimCtxt :: Netlist -> IO SimCtxt
mkSimCtxt nl = do resetHandles <- mapM mkSignalHandle $ elems nl
                  let resetArr = listArray (bounds nl) resetHandles
                  currentArr <- thaw resetArr
                  nextArr <- thaw resetArr
                  terminateRef <- newIORef False
                  return $ SimCtxt nl resetArr currentArr nextArr terminateRef
readCurrentHandles :: SimCtxt -> InstId -> IO [SignalHandle]
readCurrentHandles ctxt@SimCtxt{..} = readArray simCurrentHandles
readCurrentSignal :: SimCtxt -> WireId -> IO Signal
readCurrentSignal ctxt@SimCtxt{..} wId@(instId, outNm) = do
  sigHandles <- readCurrentHandles ctxt instId
  let Net{..} = simNetlist ! instId
  let Just idx = primOutIndex netPrim outNm
  case sigHandles !! idx of Evaluated sig -> return sig
                            Unevaluated _ -> do evalCurrentSignal ctxt wId
                                                readCurrentSignal ctxt wId

writeCurrent :: SimCtxt -> InstId -> [SignalHandle] -> IO ()
writeCurrent SimCtxt{..} instId !sighs = do
  writeArray simCurrentHandles instId sighs
writeNext :: SimCtxt -> InstId -> [SignalHandle] -> IO ()
writeNext SimCtxt{..} instId !sighs = do
  writeArray simNextHandles instId sighs
cleanCurrent :: SimCtxt -> IO ()
cleanCurrent SimCtxt{..} = do
  simEntriesCurrent :: IOArray InstId [SignalHandle] <- thaw simResetHandles
  return ()

primIsSimRoot :: Prim -> Bool
primIsSimRoot (RegisterEn _ _) = True
primIsSimRoot (Register   _ _) = True
primIsSimRoot BRAM{}           = True
primIsSimRoot p = primIsRoot p

simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  -- DBG
  dbg do mapM_ print nl
         putStrLn "============================"
  --
  let rootSteps c = [ stepPrim n c
                    | n@Net{ netPrim = p } <- elems nl, primIsSimRoot p ]
  ctxt@SimCtxt{..} <- mkSimCtxt nl
  let evenCtxt = ctxt
  let oddCtxt  = ctxt{ simCurrentHandles = simNextHandles
                     , simNextHandles    = simCurrentHandles }
  let stepEven  = sequence_ $ rootSteps evenCtxt
  let stepOdd   = sequence_ $ rootSteps oddCtxt
  let cleanEven = cleanCurrent evenCtxt
  let cleanOdd  = cleanCurrent oddCtxt
  let loop isEven = do if isEven then stepEven >> cleanEven
                                 else stepOdd  >> cleanOdd
                       done <- readIORef simTerminate
                       -- DBG
                       dbg do putStrLn "TICK ============================ TICK"
                       when (not done) do loop $ not isEven
  loop True

-- TODO: could be more lazi and only pass desired output of the prim under eval
evalCurrentSignal :: SimCtxt -> WireId -> IO ()
evalCurrentSignal ctxt (instId, _) = stepPrim (simNetlist ctxt ! instId) ctxt

-- step a primitive for each root
-- Inputs
-- Outputs
-- mutually recursive with readInput
stepPrim :: Net -> SimCtxt -> IO ()
stepPrim net@Net{ netPrim = Display args, netInputs = en:inpts, .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do inpts' <- mapM (readInput ctxt) inpts
                     putStr . concat $ fmt args inpts'
  -- DBG
  dbg do putStrLn $ "ending stepPrim " ++ show netInstId
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
stepPrim net@Net{ netPrim = Finish, netInputs = [en], .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do writeIORef (simTerminate ctxt) True
  -- DBG
  dbg do putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = Assert msg, netInputs = [en, pred], .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do pred' <- readInput ctxt pred
                     when (pred' == 0) do putStrLn msg
                                          writeIORef (simTerminate ctxt) True
  -- DBG
  dbg do putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = Register _ _, netInputs = [inpt], .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  inpt' <- readInput ctxt inpt
  writeNext ctxt netInstId [Evaluated inpt']
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{ netPrim = RegisterEn _ _
                , netInputs = [en, inpt]
                , .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do inpt' <- readInput ctxt inpt
                     writeNext ctxt netInstId [Evaluated inpt']
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{ netPrim = BRAM {ramKind = BRAMSinglePort, ..}
                , netInputs = [addr, di, we, re], ..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  [sigHandle] <- readCurrentHandles ctxt netInstId
  case sigHandle of
    Evaluated _ -> return ()
    Unevaluated (ValueBRAM val bramArray) -> do
      -- first, evaluate value for current cycle
      writeCurrent ctxt netInstId [Evaluated val]
      -- then, prepare thunk for next cycle
      re' <- readInput ctxt re
      when (re' /= 0) do
        addr' <- readInput ctxt addr
        readVal <- readArray bramArray addr'
        let bramThunk = Unevaluated $ ValueBRAM readVal bramArray
        writeNext ctxt netInstId [bramThunk]
      we' <- readInput ctxt we
      -- TODO: * for multiport BRAM, move before read for read to read the
      --         current write?
      --       * writes cause a change in the read bram output for the given
      --         port?
      when (we' /= 0) do addr' <- readInput ctxt addr
                         di' <- readInput ctxt di
                         writeArray bramArray addr' di'
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{ netPrim = BRAM {ramKind = BRAMDualPort, ..}
                , netInputs = [rdAddr, wrAddr, di, we, re], ..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  [sigHandle] <- readCurrentHandles ctxt netInstId
  case sigHandle of
    Evaluated _ -> return ()
    Unevaluated (ValueBRAM val bramArray) -> do
      -- first, evaluate value for current cycle
      writeCurrent ctxt netInstId [Evaluated val]
      -- then, prepare thunk for next cycle
      re' <- readInput ctxt re
      when (re' /= 0) do
        rdAddr' <- readInput ctxt rdAddr
        readVal <- readArray bramArray rdAddr'
        writeNext ctxt netInstId [Unevaluated $ ValueBRAM readVal bramArray]
      we' <- readInput ctxt we
      -- TODO: * for multiport BRAM, move before read for read to read the
      --         current write?
      --       * writes cause a change in the read bram output for the given
      --         port?
      when (we' /= 0) do wrAddr' <- readInput ctxt wrAddr
                         di' <- readInput ctxt di
                         writeArray bramArray wrAddr' di'
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim Net{ netPrim = BRAM {..} } _ =
  err $ "unsupported ramKind: " ++ show ramKind
stepPrim net@Net{ netPrim = TestPlusArgs _ } ctxt = return ()
{-
stepPrim net@Net{ netPrim = Mux _ _, netInputs = sel:ins, .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  sel' <- readInput ctxt sel
  val <- readInput ctxt $ ins !! fromInteger sel'
  writeCurrent ctxt netInstId [Evaluated val]
  writeNext ctxt netInstId [Unevaluated Value]
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
-}
stepPrim net@Net{..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  ins <- mapM (readInput ctxt) netInputs
  let val = evalPrim netPrim ins
  writeCurrent ctxt netInstId [Evaluated val]
  writeNext ctxt netInstId [Unevaluated Value]
  -- DBG
  dbg do currVal <- readCurrentSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --

-- mutually recursive with stepPrim
readInput :: SimCtxt -> NetInput -> IO Signal
readInput ctxt (InputWire wId) = readCurrentSignal ctxt wId
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
