{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE BlockArguments     #-}
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
  compileSim
, runSim
) where

import Prelude
import Numeric
import Data.List
import Data.Bits
import Data.Char
import Data.Maybe
import Control.Monad
import Data.Array.ST
import Control.Monad.ST
import System.Environment
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Array.IArray as IArray

import Blarney.Netlist
import Blarney.Core.Utils
import Blarney.Misc.MonadLoops
import Blarney.Core.Opts

-- debugging facilities
--import Debug.Trace
--debugEnabled = True
--dbg act = when debugEnabled act
-- debugging facilities

-- file-wide helpers
err str = error $ "Blarney.Backend.Simulation: " ++ str
simDontCare = 0
--lstIdx lst idx msg | idx >= length lst = err msg
--lstIdx lst idx _ = lst !! idx

-- | Simulate a 'Simulator' until a 'Finish' 'Prim' is triggered.
runSim :: Simulator -- ^ the 'Simulator' to simulate, compiled with 'compileSim'
       -> SignalMap -- ^ the input signals to the 'Simulator'
       -> IO SignalMap
runSim sim ins = do
  -- simply bind names
  let SimulatorIfc{..} = sim ins
  -- here, while the termination stream does not indicate the end of the
  -- simulation, we apply all effects from the current simulation cycle and
  -- also print TICK
  --mapM_ (\(effect, end) -> effect {->> print ("TICK: " ++ show end)-})
  --      (zip simEffect simTerminate)
  sequence simEffect
  return simOutputs

-- | A function to turn a 'Netlist' into a 'Simulator'
compileSim :: Map.Map String Simulator
           -- ^ existing 'Simulator's (for nested 'Custom')
           -> Netlist
           -- ^ 'Netlist' for the circuit to compile a 'Simulator' for
           -> IO Simulator
compileSim allSims originalNl = do
  -- prepare compilation context
  ctxt <- mkCompCtxt originalNl
  -- handle on the netlist
  let nl = compNl ctxt
  {- DBG -}
  --dbg do mapM_ print nl
  --       putStrLn "============================"
  {- DBG -}
  -- For this layer of Netlist
  return \currentIns -> runST mdo
    -- signal memoization table
    memo <- newListArray (IArray.bounds nl)
                         [ replicate (length $ primOutputs p) Nothing
                         | Net { netPrim = p } <- IArray.elems nl ]
    -- compile internal Custom related signals
    (childrenIds, childrenInputs, childrenOutIfcs) <- unzip3 <$> sequence
      [ do ins <- compileCustomInputs ctxt memo currentIns childrenOutputs n
           return (netInstId, ins, (allSims Map.! customName) ins)
      | n@Net{ netPrim = Custom{..}, ..} <- IArray.elems nl ]
    let childrenEffects = map sequence_ $ transpose $
                            repeat (return ()) : map simEffect childrenOutIfcs
    let childrenTerminates = map simTerminate childrenOutIfcs
    let childrenOutputs =
          IntMap.fromList $ zipWith (\i ifc -> (i, simOutputs ifc))
                                    childrenIds childrenOutIfcs
    -- compile simulation termination stream
    terminationStreams <- sequence
      [ compileTermination ctxt memo currentIns childrenOutputs n
      | n@Net{ netPrim = p } <- IArray.elems nl, case p of Finish   -> True
                                                           Assert _ -> True
                                                           _        -> False ]
    let endStreams = repeat False : (terminationStreams ++ childrenTerminates)
    let endStreams' = takeWhileInclusive not (map or $ transpose endStreams)
    let truncate :: [a] -> [a]
        truncate = map snd . zip endStreams'
    -- compile simulation effects streams
    allEffectStreams <- sequence
      [ compileSimEffect ctxt memo currentIns childrenOutputs n
      | n@Net{ netPrim = p } <- IArray.elems nl, case p of Display _ -> True
                                                           _         -> False ]
    let effectStreams = map sequence_ $ transpose $
                          repeat (return ()) : allEffectStreams
    let effectStreams' = truncate $ zipWith (>>) effectStreams childrenEffects
    -- compile simulation outputs streams
    outputStreams <- sequence
      [ do outs <- compileOutputSignal ctxt memo currentIns childrenOutputs o
           return (nm, truncate outs)
      | o@Net{ netPrim = Output _ nm } <- IArray.elems nl ]
    -- wrap compilation result into a SimulatorIfc
    return SimulatorIfc { simEffect    = effectStreams'
                        , simTerminate = endStreams'
                        , simOutputs   = Map.fromList outputStreams }
  where takeWhileInclusive _ [] = []
        takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                                 else []

--------------------------------------------------------------------------------
-- helper types

-- | A type representing the value of a 'Signal' on a given cycle
type Sample = Integer

-- | A type representing a signal
type Signal = [Sample]

-- | Context for compilation
data CompCtxt =
  CompCtxt { compArgs      :: [String]
           , compInitBRAMs :: Map.Map InstId (Map.Map Sample Sample)
           , compNl        :: Netlist }

-- | create a 'CompCtxt' from a 'Netlist'
mkCompCtxt :: Netlist -> IO CompCtxt
mkCompCtxt nl = do
  -- get command line arguments
  args <- getArgs
  -- get the BRAM initial contents
  initBRAMs <- sequence [ prepBRAMContent ramInitFile >>= return . (,) instId
                        | Net { netPrim = BRAM {..}
                              , netInstId = instId } <- IArray.elems nl ]
  return $ CompCtxt { compArgs = args
                    , compInitBRAMs = Map.fromList initBRAMs
                    , compNl = nl }
  where a2i = fst . head . readHex
        prepBRAMContent (Just f) = do
          initContent <- lines <$> readFile f
          return $ Map.fromList (zip [0..] $  a2i <$> initContent)
        prepBRAMContent Nothing = return Map.empty

-- | a dictionary of named 'Signal's
type SignalMap = Map.Map String Signal

-- | A 'Simulator''s interface
data SimulatorIfc = SimulatorIfc {
    -- | set of streams of effects
    simEffect    :: [IO ()]
    -- | stream of booleans indicating termination
  , simTerminate :: [Bool]
    -- | dictionary of output signals
  , simOutputs   :: SignalMap }

-- | A type defining the interface to a simulator
type Simulator = SignalMap -> SimulatorIfc

--------------------------------------------------------------------------------
-- netlist compilation utilities

-- | Array to memoize 'Signal' during compilation
type MemoSignals s = STArray s InstId [Maybe Signal]

-- | compile helper functions
type CompFun s a b = CompCtxt -> MemoSignals s
                     -> SignalMap -> IntMap.IntMap SignalMap
                     -> a
                     -> ST s b

-- | compile the 'SignalMap' of inputs to the given 'Custom' 'Net'
compileCustomInputs :: CompFun s Net SignalMap
compileCustomInputs ctxt memo currentIns childrenOutputs
                    Net{ netPrim = Custom{..}, ..} = do
  ins <- mapM (compileNetInputSignal ctxt memo currentIns childrenOutputs)
              netInputs
  return $ Map.fromList $ zip (map fst customInputs) ins

-- | compile the '[IO ()]' stream of simulation effects for a given simulation
--   effect capable 'Net' (currently, only 'Display' nets)
compileSimEffect :: CompFun s Net [IO ()]
compileSimEffect ctxt memo currentIns childrenOutputs
                 Net{ netPrim = Display args, .. } = do
  ins <- mapM (compileNetInputSignal ctxt memo currentIns childrenOutputs)
              netInputs
  return $ map (\(en:inpts) ->
                 when (en /= 0) do putStr . concat $ fmt args inpts)
               (transpose ins)
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
compileTermination ctxt memo currentIns childrenOutputs
                   Net{ netPrim = Finish, netInputs = [en] } = do
  en' <- compileNetInputSignal ctxt memo currentIns childrenOutputs en
  return $ map (/= 0) en'

-- | compile the output 'Signal' for a given output 'Net'
--   (currently not supported)
compileOutputSignal :: CompFun s Net Signal
compileOutputSignal ctxt memo currentIns childrenOutputs
                    Net{ netPrim = Output _ _, ..} = do
  compileNetInputSignal ctxt memo currentIns childrenOutputs (head netInputs)

-- | compile the 'Signal' associated with a 'WireId' (memoized)
compileWireIdSignal :: CompFun s WireId Signal
compileWireIdSignal ctxt@CompCtxt{..} memo currentIns childrenOutputs
                    (instId, outNm) = do
  thunks <- readArray memo instId
  let net = getNet compNl instId
  case primOutIndex (netPrim net) outNm of
    Just idx ->
      case thunks !! idx of
        Just signal -> return signal
        Nothing -> mdo
          -- first write the memoization array
          let thunks' = take idx thunks ++ Just signal : drop (idx+1) thunks
          writeArray memo instId thunks'
          -- then compile the signal
          signal <- compileNetIndexedOuptutSignal ctxt memo currentIns
                                                  childrenOutputs (net, idx)
          return signal
    Nothing -> err $ "unknown output " ++ show outNm ++ " for net " ++ show net

-- | compile the 'Signal' associated with an indexed output of a given 'Net'
--   (not memoized)
compileNetIndexedOuptutSignal :: CompFun s (Net, Int) Signal
compileNetIndexedOuptutSignal ctxt memo currentIns childrenOutputs
                              (Net{..}, idx) = do
  ins <- mapM (compileNetInputSignal ctxt memo currentIns childrenOutputs)
              netInputs
  return $ compilePrim ctxt currentIns childrenOutputs netInstId netPrim ins idx

-- | compile the 'Signal' associated with a 'NetInput' (not memoized)
compileNetInputSignal :: CompFun s NetInput Signal
compileNetInputSignal ctxt memo currentIns childrenOutputs (InputWire wId) = do
  compileWireIdSignal ctxt memo currentIns childrenOutputs wId
compileNetInputSignal ctxt memo currentIns childrenOutputs
                      (InputTree prim ins) = do
  ins' <- mapM (compileNetInputSignal ctxt memo currentIns childrenOutputs) ins
  let instID = err "no InstId for InputTree Prims"
  -- TODO check that we only eval single output primitives
  return $ compilePrim ctxt currentIns childrenOutputs instID prim ins' 0

-- | merges a new sample of provided width into an old sample of same width
--   according to the provided byte enable
mergeWithBE :: Width -> Sample -> Sample -> Sample -> Sample
mergeWithBE w be new old = (newMask .&. new) .|. (oldMask .&. old)
  where newMask = foldl (\a x -> shiftL a 8 .|. if x then 0xff else 0x00) 0 beLst
        oldMask = foldl (\a x -> shiftL a 8 .|. if x then 0x00 else 0xff) 0 beLst
        beLst = reverse $ testBit be <$> [0..(log2ceil w - 1)]

-- | Describe how a primitive, given some input 'Signal's produces its output
--   'Signals' (most often only the one output 'Signal' returned as a singleton
--   list)
--   Note:
--     * the CompCtxt argument is needed for primitives such as TestPlusArgs
--     * the InstId argument is needed to identify 'Net' initialization data
compilePrim :: CompCtxt -> SignalMap -> IntMap.IntMap SignalMap
            -> InstId -> Prim -> [Signal] -> Int
            -> Signal
compilePrim CompCtxt{..} currentIns childrenOutputs
            instId prim ins idx = case (prim, ins) of
  -- no input primitives
  (TestPlusArgs plsArg, []) ->
    repeat $ if '+' : plsArg `elem` compArgs then 1 else 0
  (Const w val, []) -> repeat $ clamp w $ fromInteger val
  (DontCare w, []) -> repeat simDontCare
  -- stateful primitives
  (Register i _, [inpts]) -> fromMaybe simDontCare i : inpts
  (RegisterEn i _, [ens, inpts]) ->
    scanl f (fromMaybe simDontCare i) (zip ens inpts)
    where f prev (en, inpt) = if en /= 0 then inpt else prev
  (Input _ nm, _) -> currentIns Map.! nm
  (Custom{..}, _) -> childOuts Map.! nm
    where childOuts = childrenOutputs IntMap.! instId
          nm = fst $ (primOutputs prim) !! idx
  (BRAM{ ramKind = BRAMSinglePort, .. }, inpts@(addrS:_:weS:reS:_) ) ->
    zipWith3 doRead delayedAddrS delayedWeS bramContentS
    where delayedAddrS = scanl (\prv (a, re) -> if re /= 0 then a else prv)
                               simDontCare
                               (zip (clamp ramAddrWidth <$> addrS) reS)
          delayedWeS = simDontCare : weS
          doRead addr we cntnt
            | we /= 0 = simDontCare
            | otherwise = fromMaybe simDontCare $ Map.lookup addr cntnt
          bramContentS = scanl t (compInitBRAMs Map.! instId)
                                 (transpose inpts)
          t prev (addr:di:we:re:m_be)
            | we /= 0 =
              Map.alter (wrt m_be di) (clamp ramAddrWidth addr) prev
            | otherwise = prev
          wrt m_be new m_old
            | ramHasByteEn, [be] <- m_be =
              Just $ mergeWithBE ramDataWidth be new old
            | otherwise = Just $ clamp ramDataWidth new
            where old = case m_old of Just x  -> x
                                      Nothing -> simDontCare
  (BRAM{ ramKind = BRAMDualPort, .. }, inpts@(rdAddrS:wrAddrS:_:weS:reS:_) ) ->
    zipWith4 doRead delayedRdAddrS delayedWrAddrS delayedWeS bramContentS
    where delayedRdAddrS = scanl (\prv (a, re) -> if re /= 0 then a else prv)
                                 simDontCare
                                 (zip (clamp ramAddrWidth <$> rdAddrS) reS)
          delayedWrAddrS = scanl (\prv (a, we) -> if we /= 0 then a else prv)
                                 simDontCare
                                 (zip (clamp ramAddrWidth <$> wrAddrS) weS)
          delayedWeS = simDontCare : weS
          doRead rdAddr wrAddr we cntnt
            | we /= 0, rdAddr == wrAddr = simDontCare
            | otherwise = fromMaybe simDontCare $ Map.lookup rdAddr cntnt
          bramContentS = scanl t (compInitBRAMs Map.! instId)
                                 (transpose inpts)
          t prev (_:wrAddr:di:we:_:m_be)
            | we /= 0 =
              Map.alter (wrt m_be di) (clamp ramAddrWidth wrAddr) prev
            | otherwise = prev
          wrt m_be new m_old
            | ramHasByteEn, [be] <- m_be =
              Just $ mergeWithBE ramDataWidth be new old
            | otherwise = Just $ clamp ramDataWidth new
            where old = case m_old of Just x  -> x
                                      Nothing -> simDontCare
  (BRAM{ ramKind = BRAMTrueDualPort, .. }, inpts@(addrAs:addrBs:_:_:weAs:weBs:reAs:reBs:_)) ->
    [ zipWith4 doRead delayedAddrAs delayedAddrBs delayedWeAs bramContentS
    , zipWith4 doRead delayedAddrBs delayedAddrAs delayedWeBs bramContentS ] !! idx
    where delayedAddrAs = scanl (\prv (a, re) -> if re /= 0 then a else prv)
                                simDontCare
                                (zip (clamp ramAddrWidth <$> addrAs) reAs)
          delayedAddrBs = scanl (\prv (a, re) -> if re /= 0 then a else prv)
                                simDontCare
                                (zip (clamp ramAddrWidth <$> addrBs) reBs)
          delayedWeAs = simDontCare : weAs
          delayedWeBs = simDontCare : weBs
          doRead rdAddr wrAddr we cntnt
            | we /= 0, rdAddr == wrAddr = simDontCare
            | otherwise = fromMaybe simDontCare $ Map.lookup rdAddr cntnt
          bramContentS = scanl t (compInitBRAMs Map.! instId)
                                 (transpose inpts)
          t prev (addrA:addrB:diA:diB:weA:weB:_:_:m_bes)
            | weA /= 0, weB /= 0, addrA == addrB = Map.insert addrA cnflct prev
            | otherwise = go (go prev addrA diA weA beA) addrB diB weB beB
            where ones = complement zeroBits
                  [beA, beB] = if ramHasByteEn then m_bes else [ones, ones]
                  cnflct = err "BRAM value written by conflicting writes"
          go prev addr di we be
            | we /= 0 =
              Map.alter f (clamp ramAddrWidth addr) prev
            | otherwise = prev
            where f (Just x) = Just $ mergeWithBE ramDataWidth be di x
                  f  Nothing = Just $ mergeWithBE ramDataWidth be di simDontCare
  -- special cased primitives (handled by fall through case but explicitly
  -- pattern matched here for better performance)
  (Add _, [x, y]) -> evalBinOp x y
  (Sub _, [x, y]) -> evalBinOp x y
  (Mul{}, [x, y]) -> evalBinOp x y
  (Div _, [x, y]) -> evalBinOp x y
  (Mod _, [x, y]) -> evalBinOp x y
  (Not _,    [x]) -> evalUnOp x
  (And _, [x, y]) -> evalBinOp x y
  (Or  _, [x, y]) -> evalBinOp x y
  (Xor _, [x, y]) -> evalBinOp x y
  (ShiftLeft _ _, [x, y]) -> evalBinOp x y
  (ShiftRight _ _, [x, y]) -> evalBinOp x y
  (ArithShiftRight _ _, [x, y]) -> evalBinOp x y
  (Equal _, [x, y]) -> evalBinOp x y
  (NotEqual _, [x, y]) -> evalBinOp x y
  (LessThan _, [x, y]) -> evalBinOp x y
  (LessThanEq _, [x, y]) -> evalBinOp x y
  (ReplicateBit _, [x]) -> evalUnOp x
  (ZeroExtend _ _, [x]) -> evalUnOp x
  (SignExtend _ _, [x]) -> evalUnOp x
  (SelectBits _ _ _, [x]) -> evalUnOp x
  (Concat _ _, [x, y]) -> evalBinOp x y
  (Identity _, [x]) -> evalUnOp x
  (Mux _ _ w, [ss, i0, i1]) ->
    zipWith3 (\s x y -> if s == 0 then x else y) ss i0 i1
  -- fall through case
  _ -> (transpose $ eval <$> transpose ins) !! idx
  where eval = primSemEval prim
        evalUnOp i0 = concatMap eval ((:[]) <$> i0)
        evalBinOp i0 i1 = zipWith (\x y -> head $ eval [x, y]) i0 i1
