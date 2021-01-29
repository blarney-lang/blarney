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
--import Data.Array
import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import System.Environment

import Blarney.Netlist
import Blarney.Misc.MonadLoops

withW :: Int -> Integer -> Integer
withW w i = (bit w - 1) .&. i

data SimEntry = SimSignal Integer
              | SimBRAM { bramAddrA :: IORef Integer
                        , bramAddrB :: IORef Integer
                        , bramArray :: IOArray Integer Integer }
mkSimEntry :: Net -> IO SimEntry
mkSimEntry Net{ netPrim = (Const _ val) } = return $ SimSignal val
mkSimEntry Net{ netPrim = (DontCare _) } = return $ SimSignal 0 -- TODO other value?
mkSimEntry Net{ netPrim = (Register val _) } = return $ SimSignal val
mkSimEntry Net{ netPrim = (RegisterEn val _) } = return $ SimSignal val
mkSimEntry Net{ netPrim = BRAM{..} } = do
  addrARef <- newIORef 0
  addrBRef <- newIORef 0
  firstBytes <- case ramInitFile of
                  Just f -> do initContent <- lines <$> readFile f
                               return $ fst . head . readHex <$> initContent
                  _ -> return []
  dataArray <- newListArray (0, 2^ramAddrWidth-1)
                            (firstBytes ++ repeat errUninit)
  return $ SimBRAM addrARef addrBRef dataArray
  --where errUninit = error "Blarney.Backend.Simulation: uninitialized memory"
  where errUninit = 0
mkSimEntry Net{ netInstId = i } = return . SimSignal $ errUninit i
  where errUninit i =
          error $ "Blarney.Backend.Simulation: uninitialized value, instId "
                  ++ show i

data SimCtxt = SimCtxt { simEntries   :: IOArray InstId SimEntry
                       , simOnStep    :: IORef [IO ()]
                       , simTerminate :: IORef Bool }
mkSimCtxt :: Netlist -> IO SimCtxt
mkSimCtxt nl = do
  entries <- mapM mkSimEntry $ elems nl
  entriesArray <- newListArray (bounds nl) entries
  onStepRef <- newIORef []
  terminateRef <- newIORef False
  return SimCtxt{ simEntries   = entriesArray
                , simOnStep    = onStepRef
                , simTerminate = terminateRef }

readEntry :: SimCtxt -> InstId -> IO Integer
readEntry ctxt@SimCtxt{..} instId = do
  simEntry <- readArray simEntries instId
  case simEntry of SimSignal sig -> return sig
                   SimBRAM {..} -> readBRAMPortA ctxt instId

readSignal :: SimCtxt -> InstId -> IO Integer
readSignal SimCtxt{..} instId = do
  simEntry <- readArray simEntries instId
  case simEntry of SimSignal sig -> return sig
                   _ -> error "expected SimSignal"
writeSignal :: SimCtxt -> InstId -> Integer -> IO ()
writeSignal SimCtxt{..} instId !val = do
  simEntry <- readArray simEntries instId
  case simEntry of SimSignal _ -> writeArray simEntries instId (SimSignal val)
                   _ -> error "expected SimSignal"
readBRAMPortA :: SimCtxt -> InstId -> IO Integer
readBRAMPortA SimCtxt{..} instId = do
  simEntry <- readArray simEntries instId
  case simEntry of SimBRAM {..} -> readArray bramArray =<< readIORef bramAddrA
                   _ -> error "expected SimBRAM"
readBRAMPortB :: SimCtxt -> InstId -> IO Integer
readBRAMPortB SimCtxt{..} instId = do
  simEntry <- readArray simEntries instId
  case simEntry of SimBRAM {..} -> readArray bramArray =<< readIORef bramAddrB
                   _ -> error "expected SimBRAM"
writeBRAM :: SimCtxt -> InstId -> Integer -> Integer -> IO ()
writeBRAM SimCtxt{..} instId !addr !value = do
  simEntry <- readArray simEntries instId
  case simEntry of SimBRAM {..} -> writeArray bramArray addr value
                   _ -> error "expected SimBRAM"
writeBRAMAddrPortA :: SimCtxt -> InstId -> Integer -> IO ()
writeBRAMAddrPortA SimCtxt{..} instId !addr = do
  simEntry <- readArray simEntries instId
  case simEntry of SimBRAM {..} -> writeIORef bramAddrA addr
                   _ -> error "expected SimBRAM"
writeBRAMAddrPortB :: SimCtxt -> InstId -> Integer -> IO ()
writeBRAMAddrPortB SimCtxt{..} instId !addr = do
  simEntry <- readArray simEntries instId
  case simEntry of SimBRAM {..} -> writeIORef bramAddrB addr
                   _ -> error "expected SimBRAM"
onStep :: SimCtxt -> IO () -> IO ()
onStep SimCtxt{..} act = modifyIORef simOnStep (\acts -> act:acts)
isOver :: SimCtxt -> IO Bool
isOver SimCtxt{..} = readIORef simTerminate
terminate :: SimCtxt -> IO ()
terminate SimCtxt{..} = writeIORef simTerminate True
step :: SimCtxt -> IO ()
step SimCtxt{..} = do extraActs <- readIORef simOnStep
                      sequence_ extraActs -- TODO possibly reverse...
                      writeIORef simOnStep []

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

evalInput :: SimCtxt -> NetInput -> IO Integer
evalInput ctxt (InputWire (instId, outNm)) = readEntry ctxt instId
evalInput ctxt (InputTree prim ins) = do inSigs <- mapM (evalInput ctxt) ins
                                         return $ evalPrim prim inSigs
-- | Get a step function for a given primitive
primStep :: Net -> SimCtxt -> IO ()
primStep Net{ netPrim   = Register _ _
            , netInstId = instId
            , netInputs = [inpt] } ctxt = onStep ctxt do
  inSig <- evalInput ctxt inpt
  writeSignal ctxt instId inSig
primStep Net{ netPrim   = RegisterEn _ _
            , netInstId = instId
            , netInputs = [en, inpt] } ctxt = onStep ctxt do
  enSig <- evalInput ctxt en
  when (enSig /= 0) $ do inSig <- evalInput ctxt inpt
                         writeSignal ctxt instId inSig
primStep Net{ netPrim = BRAM {ramKind = BRAMSinglePort, ..}
            , netInstId = instId
            , netInputs = [addr, di, we, re] } ctxt = do
  reSig <- evalInput ctxt re
  when (reSig /= 0) $ onStep ctxt do addrSig <- evalInput ctxt addr
                                     writeBRAMAddrPortA ctxt instId addrSig
  weSig <- evalInput ctxt we
  when (weSig /= 0) $ onStep ctxt do addrSig <- evalInput ctxt addr
                                     diSig <- evalInput ctxt di
                                     writeBRAMAddrPortA ctxt instId addrSig
                                     writeBRAM ctxt instId addrSig diSig
primStep Net{ netPrim = BRAM {ramKind = BRAMDualPort, ..}
            , netInstId = instId
            , netInputs = [rdAddr, wrAddr, di, we, re] } ctxt = do
  reSig <- evalInput ctxt re
  when (reSig /= 0) $ onStep ctxt do addrSig <- evalInput ctxt rdAddr
                                     writeBRAMAddrPortA ctxt instId addrSig
  weSig <- evalInput ctxt we
  when (weSig /= 0) $ onStep ctxt do addrSig <- evalInput ctxt wrAddr
                                     diSig <- evalInput ctxt di
                                     writeBRAMAddrPortB ctxt instId addrSig
                                     writeBRAM ctxt instId addrSig diSig
primStep Net{ netPrim   = Display args
            , netInstId = instId
            , netInputs = en:inpts } ctxt = onStep ctxt do
  enSig<- evalInput ctxt en
  when (enSig /= 0) $ do inSigs <- mapM (evalInput ctxt) inpts
                         putStr . concat . fmt args $! inSigs
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
primStep Net{ netPrim = Finish, netInputs = [finish] } ctxt = do
  finishSig <- evalInput ctxt finish
  when (finishSig /= 0) $! terminate ctxt
primStep Net{ netPrim   = TestPlusArgs plsArg
            , netInstId = instId } ctxt = do
  args <- getArgs
  writeSignal ctxt instId if '+' : plsArg `elem` args then 1 else 0
primStep Net{ netPrim   = Assert msg
            , netInputs = [en, pred] } ctxt = do
  enSig <- evalInput ctxt en
  when (enSig /= 0) do ok <- evalInput ctxt pred
                       when (ok == 0) do putStrLn msg
                                         terminate ctxt
primStep Net{ netPrim   = prim
            , netInstId = instId
            , netInputs = ins } ctxt = do
  inSigs <- mapM (evalInput ctxt) ins
  writeSignal ctxt instId (evalPrim prim inSigs)

simulateNetlist :: Netlist -> IO ()
simulateNetlist nl = do
  ctxt <- mkSimCtxt nl
  let rawSteps :: Array InstId (IO ()) =
        listArray (bounds nl) [primStep n ctxt | n <- elems nl]
  let orderedSteps = map (rawSteps !) (topologicalSort nl)
  -- DBG
  -- let orderedSteps = map (\i -> putStrLn ("step for " ++ show i) >> rawSteps ! i) (topologicalSort nl)
  -- putStrLn $ "topo order: " ++ show (topologicalSort nl)
  -- mapM_ print nl
  --
  sequence_ orderedSteps >> step ctxt `untilM_` isOver ctxt
