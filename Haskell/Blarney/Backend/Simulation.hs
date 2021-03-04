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

-- | Simulate a 'Netlist' until a 'Finish' 'Prim' is triggered
simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  -- DBG
  dbg do mapM_ print nl
         putStrLn "============================"
  --
  -- simulation steps for Net primitives on each simulation cycle
  let stepPrims ctxt = [ stepPrim net ctxt
                       | net@Net{..} <- elems nl, primIsSimRoot netPrim ]
  -- Note: could also force evaluation of all Nets in topological order...
  ctxt <- mkSimCtxt nl
  sequence_ (stepPrims ctxt) >> stepSimCtxt ctxt
                             >> dbg (putStrLn "TICK ============================ TICK")
    `untilM_` readIORef (simTerminate ctxt)

-- | Determine whether a 'Prim' is a simulation root
primIsSimRoot :: Prim -> Bool
primIsSimRoot (RegisterEn _ _) = True
primIsSimRoot (Register   _ _) = True
primIsSimRoot BRAM{}           = True
primIsSimRoot p = primIsRoot p

-- | a simulation 'Signal' type (an 'Integer')
type Signal = Integer

-- | a simulation context for specific primitives
data PrimCtxt = NoCtxt | BRAMCtxt (IOArray Signal Signal)

-- | a simulation handle to a 'Net', that is a pair of a list of possibly
--   evaluated 'Signal's and a context for the 'Net''s primitive
type NetHandle = ([Maybe Signal], PrimCtxt)

-- create the 'NetHandle's representing a given 'Net'
mkNetHandle :: Net -> IO NetHandle
mkNetHandle Net{ netPrim = (Const _ val) }      = return ([Just val], NoCtxt)
mkNetHandle Net{ netPrim = (DontCare _) }       = return ([Just   0], NoCtxt) -- TODO other value?
mkNetHandle Net{ netPrim = (Register val _) }   = return ([Just val], NoCtxt)
mkNetHandle Net{ netPrim = (RegisterEn val _) } = return ([Just val], NoCtxt)
mkNetHandle Net{ netPrim = TestPlusArgs plsArg } = do
  args <- getArgs
  return ([Just if '+' : plsArg `elem` args then 1 else 0], NoCtxt)
mkNetHandle Net{ netPrim = BRAM{..} } = do
  firstBytes <- case ramInitFile of
                  Just f -> do initContent <- lines <$> readFile f
                               return $ fst . head . readHex <$> initContent
                  _ -> return []
  dataArray <- newListArray (0, 2^ramAddrWidth-1)
                            (firstBytes ++ repeat errUninit)
  fstVal <- readArray dataArray 0
  return case ramKind of
           BRAMTrueDualPort -> ([Just fstVal, Just fstVal], BRAMCtxt dataArray)
           _ -> ([Just fstVal], BRAMCtxt dataArray)
  --where errUninit = err "accessing uninitialized memory"
  where errUninit = 0
mkNetHandle _ = return ([Nothing], NoCtxt)

-- | Simulation Context
data SimCtxt = SimCtxt { simNetlist    :: Netlist
                       , simNetHandles :: IOArray InstId NetHandle
                       , simStepIO     :: IORef (IO ())
                       , simTerminate  :: IORef Bool }

-- | create a 'SimCtxt' simulation context from a netlist
mkSimCtxt :: Netlist -> IO SimCtxt
mkSimCtxt nl = do resetHandles <- mapM mkNetHandle $ elems nl
                  handlesArr <- newListArray (bounds nl) resetHandles
                  stepIORef <- newIORef $ return ()
                  terminateRef <- newIORef False
                  return $ SimCtxt nl handlesArr stepIORef terminateRef

-- | add an IO for the current simulation step
addIO :: SimCtxt -> IO () -> IO ()
addIO SimCtxt{..} act = modifyIORef' simStepIO (>> act)

-- | step a simulation context, performing accumulated IOs and reseting them for
--   the next simulation cycle
stepSimCtxt :: SimCtxt -> IO ()
stepSimCtxt SimCtxt{..} = do act <- readIORef simStepIO
                             act
                             writeIORef simStepIO $ return ()

-- | get the list of 'Maybe Signal' output from the net at the given 'InstId'
readThunks :: SimCtxt -> InstId -> IO [Maybe Signal]
readThunks SimCtxt{..} instId = do (thunks, _) <- readArray simNetHandles instId
                                   return thunks

-- | get the evaluated 'Signal' output from the net at the given 'InstId'
readSignal :: SimCtxt -> WireId -> IO Signal
readSignal ctxt@SimCtxt{..} wId@(instId, outNm) = do
  sigThunks <- readThunks ctxt instId
  let Net{..} = simNetlist ! instId
  let Just idx = primOutIndex netPrim outNm
  case sigThunks !! idx of Just sig -> return sig
                           Nothing  -> do evalCurrentSignal ctxt wId
                                          readSignal ctxt wId

-- | evaluate the net output signal at the give 'WireId' by invoking the
--   appropriate 'stepPrim' function
-- TODO: could be more lazi and only pass desired output of the prim under eval
evalCurrentSignal :: SimCtxt -> WireId -> IO ()
evalCurrentSignal ctxt (instId, _) = stepPrim (simNetlist ctxt ! instId) ctxt

-- | set the value of the output signal thunks of the net at the given 'InstId'
--   (useful to actually reflect evaluation of thunks)
writeThunks :: SimCtxt -> InstId -> [Maybe Signal] -> IO ()
writeThunks SimCtxt{..} instId !sigThunks = do
  (_, ctxt) <- readArray simNetHandles instId
  writeArray simNetHandles instId (sigThunks, ctxt)

-- | step a primitive for each root
-- mutually recursive with 'readInput'
stepPrim :: Net -> SimCtxt -> IO ()
stepPrim net@Net{ netPrim = Display args, netInputs = en:inpts, .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do inpts' <- mapM (readInput ctxt) inpts
                     addIO ctxt do putStr . concat $ fmt args inpts'
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
  when (en' /= 0) do addIO ctxt do writeIORef (simTerminate ctxt) True
  -- DBG
  dbg do putStrLn $ "ending stepPrim " ++ show netInstId
  --
stepPrim net@Net{ netPrim = Assert msg, netInputs = [en, pred], .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  en' <- readInput ctxt en
  when (en' /= 0) do
    pred' <- readInput ctxt pred
    when (pred' == 0) do addIO ctxt do putStrLn msg
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
  addIO ctxt do writeThunks ctxt netInstId [Just inpt']
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
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
                     addIO ctxt do writeThunks ctxt netInstId [Just inpt']
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{ netPrim = BRAM {ramKind = BRAMSinglePort, ..}
                , netInputs = [addr, di, we, re], ..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  (_, BRAMCtxt bramArray) <- readArray (simNetHandles ctxt) netInstId
  -- prepare thunk for next cycle on read
  re' <- readInput ctxt re
  when (re' /= 0) do
    addr' <- readInput ctxt addr
    readVal <- readArray bramArray addr'
    addIO ctxt do writeThunks ctxt netInstId [Just readVal]
  -- on write, perform array update
  we' <- readInput ctxt we
  -- TODO: * for multiport BRAM, move before read for read to read the
  --         current write?
  --       * writes cause a change in the read bram output for the given
  --         port?
  when (we' /= 0) do addr' <- readInput ctxt addr
                     di' <- readInput ctxt di
                     addIO ctxt do writeArray bramArray addr' di'
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{ netPrim = BRAM {ramKind = BRAMDualPort, ..}
                , netInputs = [rdAddr, wrAddr, di, we, re], ..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  (_, BRAMCtxt bramArray) <- readArray (simNetHandles ctxt) netInstId
  -- prepare thunk for next cycle on read
  re' <- readInput ctxt re
  when (re' /= 0) do
    rdAddr' <- readInput ctxt rdAddr
    readVal <- readArray bramArray rdAddr'
    addIO ctxt do writeThunks ctxt netInstId [Just readVal]
  -- on write, perform array update
  we' <- readInput ctxt we
  -- TODO: * for multiport BRAM, move before read for read to read the
  --         current write?
  --       * writes cause a change in the read bram output for the given
  --         port?
  when (we' /= 0) do wrAddr' <- readInput ctxt wrAddr
                     di' <- readInput ctxt di
                     addIO ctxt do writeArray bramArray wrAddr' di'
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim Net{ netPrim = BRAM {..} } _ =
  err $ "unsupported ramKind: " ++ show ramKind
stepPrim net@Net{ netPrim = TestPlusArgs _ } ctxt = return ()
stepPrim net@Net{ netPrim = Mux _ _, netInputs = sel:ins, .. } ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  sel' <- readInput ctxt sel
  val <- readInput ctxt $ ins !! fromInteger sel'
  writeThunks ctxt netInstId [Just val]
  addIO ctxt do writeThunks ctxt netInstId [Nothing]
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --
stepPrim net@Net{..} ctxt = do
  -- DBG
  dbg do putStrLn $ "start stepPrim " ++ show netInstId
         print net
  --
  ins <- mapM (readInput ctxt) netInputs
  let val = evalPrim netPrim ins
  writeThunks ctxt netInstId [Just val]
  addIO ctxt do writeThunks ctxt netInstId [Nothing]
  -- DBG
  dbg do currVal <- readSignal ctxt (netInstId, Nothing)
         putStrLn $ "ending stepPrim " ++ show netInstId ++ " with currVal " ++ show currVal
  --

-- mutually recursive with stepPrim
readInput :: SimCtxt -> NetInput -> IO Signal
readInput ctxt (InputWire wId) = readSignal ctxt wId
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
