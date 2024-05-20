{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.NewSMT
Description : SMT generation, improved
Copyright   : (c) Victor Miquel, 2024
License     : MIT
Stability   : experimental

Verify circuit properties using SMT solver.

TLDR: Use `verifyDefault`.

- the `verifyOffline` family can be used to produce SMT-LIB input files.
- the `verifyLive` family allows one to run simple verification schemes.
- `verifyCircuit` provides concurrent verification.
- `verifyDefault` is general-purpose, efficient verification scheme.
-}

module Blarney.Backend.NewSMT (
  Blarney.Backend.NewSMT.Verbosity (..)
, Blarney.Backend.NewSMT.VerifyResult
, Blarney.Backend.NewSMT.OutputType
, Blarney.Backend.NewSMT.Writer
, Blarney.Backend.NewSMT.write_nothing
, Blarney.Backend.NewSMT.write_screen
, Blarney.Backend.NewSMT.VerifConf (..)
, Blarney.Backend.NewSMT.vconfQuiet
, Blarney.Backend.NewSMT.vconfDefault
, Blarney.Backend.NewSMT.vconfDebug
, Blarney.Backend.NewSMT.FixedConf (..)
, Blarney.Backend.NewSMT.fconfCombinational
, Blarney.Backend.NewSMT.IncrementalConf (..)
, Blarney.Backend.NewSMT.iconfDefault

, Blarney.Backend.NewSMT.verifyOfflineFixed
, Blarney.Backend.NewSMT.verifyOfflineQIFixed
, Blarney.Backend.NewSMT.verifyLiveBounded
, Blarney.Backend.NewSMT.verifyLiveFixed
, Blarney.Backend.NewSMT.verifyLiveQIFixed
, Blarney.Backend.NewSMT.verifyLiveIncremental

, Blarney.Backend.NewSMT.checkBounded
, Blarney.Backend.NewSMT.checkRestrInd
, Blarney.Backend.NewSMT.checkQuantInd
, Blarney.Backend.NewSMT.incrSeq
, Blarney.Backend.NewSMT.proofPartGenerator
, Blarney.Backend.NewSMT.verifyCircuit

, Blarney.Backend.NewSMT.checkAuto
, Blarney.Backend.NewSMT.checkFixed
, Blarney.Backend.NewSMT.debugAuto
, Blarney.Backend.NewSMT.debugFixed
) where

-- Standard imports
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import System.Exit
import System.Process
import Text.PrettyPrint
import Text.Printf
import Data.Array.IArray
import Prelude hiding ((<>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent.Async

-- Blarney imports
import Blarney.Netlist
import Blarney.Core.Interface
import Blarney.Misc.ANSIEscapeSequences

-- | Verbosity level
data Verbosity = Verbose
               | Info
               | Quiet

-- | Write without newline and flush stdout
putStrFlush :: String -> IO ()
putStrFlush x = putStr x >> hFlush stdout

sayVerboseLn :: Verbosity -> String -> IO ()
sayVerboseLn verb x = case verb of
  Verbose -> putStrLn x
  Info -> return ()
  Quiet -> return ()

sayInfoLn :: Verbosity -> String -> IO ()
sayInfoLn verb x = case verb of
  Verbose -> putStrLn x
  Info -> putStrLn x
  Quiet -> return ()

sayVerboseFlush :: Verbosity -> String -> IO ()
sayVerboseFlush verb x = case verb of
  Verbose -> putStrFlush x
  Info -> return ()
  Quiet -> return ()

sayInfoFlush :: Verbosity -> String -> IO ()
sayInfoFlush verb x = case verb of
  Verbose -> putStrFlush x
  Info -> putStrFlush x
  Quiet -> return ()

-- | Outcome of verification
data VerifyResult = Falsifiable  -- A counter example was found
                  | Verified     -- The property holds
                  | Insufficient -- More assumptions are needed

-- | Output kinds, used for filtering out unwanted parts
data OutputType = SMTCommand -- An SMT command that must be executed
                | SMTComment -- An SMT comment that can be ommited
                | SMTEcho -- An SMT echo command that can be ommited
                | User -- A string for the user to read (not for SMT solver)

-- | Signature of functions that perform output depending on the output type
type Writer = OutputType -> Doc -> IO ()

-- | Writer that never writes
write_nothing :: Writer
write_nothing _ _ = return ()

-- | Writer that always writes
write_screen :: Writer
write_screen _ = putStrLn . render

-- | Writer modifier: only write SMT output
write_smt :: (Doc -> IO ()) -> Writer
write_smt w t x = case t of
  SMTCommand -> w x
  SMTComment -> w x
  SMTEcho -> w x
  _ -> return ()

-- | Writer modifier: only write mandatory SMT output
write_smt_commands :: (Doc -> IO ()) -> Writer
write_smt_commands w t x = case t of
  SMTCommand -> w x
  _ -> return ()

-- | Generic Verification Config
data VerifConf = VerifConf {
  write :: Writer
, giveModel :: Bool
}

vconfQuiet = VerifConf { write=write_nothing, giveModel=False }
vconfDefault = VerifConf { write=write_nothing, giveModel=True }
vconfDebug = VerifConf { write=write_screen, giveModel=True }

-- | Verification Config for fixed depth
data FixedConf = FixedConf {
  depth :: Int
}

fconfCombinational = FixedConf { depth=0 }

-- | Verification Config for incremental depth
data IncrementalConf = IncrementalConf {
  limit :: Maybe Int
}

iconfDefault = IncrementalConf { limit = Nothing }

data NetConf = NetConf {
  stateFields :: [(InstId, Doc, Int)]
, stateFieldsInit :: [[(Int, Maybe Integer)]]
, inputFields :: [(InstId, Doc, Int)]
, initName :: Doc -> Doc
, lastName :: Doc -> Doc
, transitionName :: Doc
}

mkNetConf :: Netlist -> Net -> NetConf
mkNetConf netlist net =
  NetConf {
    stateFields = stateFields
  , stateFieldsInit = stateFieldsInit
  , inputFields = inputFields
  , initName = \field -> (text $ "state_init" ++ (show $ netInstId net) ++ "_") <> field
  , lastName = \field -> (text $ "state_last" ++ (show $ netInstId net) ++ "_") <> field
  , transitionName = text $ "transition" ++ (show $ netInstId net)
  }
  where
    (stateFields, stateFieldsInit) = unzip $ catMaybes [
      case netPrim of
        RegisterEn init w -> Just ((netInstId, fmtWire netlist (netInstId, Nothing), w), init)
        Register init w -> Just ((netInstId, fmtWire netlist (netInstId, Nothing), w), init)
        _ -> Nothing
      | Net{..} <- elems netlist
      , elem netInstId support]
    inputFields = [(netInstId, fmtWire netlist (netInstId, Nothing), w) | Net{netPrim=Input w _, netInstId} <- elems netlist, elem netInstId support]
               ++ [(netInstId, fmtWire netlist (netInstId, Nothing), w) | Net{netPrim=DontCare w, netInstId} <- elems netlist, elem netInstId support]
    support = partialTopologicalSort netlist $ netInstId net

data SeqConf = SeqConf {
  stateName :: Int -> Doc -> Doc
, inputName :: Int -> Doc -> Doc
, okName :: Int -> Doc
, distinctStates :: Bool
}

mkSeqConf :: String -> Bool -> SeqConf
mkSeqConf suffix distinctStates = SeqConf {
  stateName = \n field -> (text $ "state_" ++ suffix ++ show n ++ "_") <> field
, inputName = \n field -> (text $ "input_" ++ suffix ++ show n ++ "_") <> field
, okName = \n -> text $ "ok_" ++ suffix ++ show n
, distinctStates = distinctStates
}

boundedConf :: SeqConf
boundedConf = mkSeqConf "head" False

inductionConf :: Bool -> SeqConf
inductionConf restricted = mkSeqConf "tail" restricted


-- SMT helpers --

smtText :: String -> Doc
smtText = text

smtInt :: Int -> Doc
smtInt = int

smtBVLit :: Int -> Integer -> Doc
smtBVLit w n = text $ printf ("#b%0" ++ show w ++ "b") n

smtOpN :: String -> [Doc] -> Doc
smtOpN op xs = smtGroup (text op : xs)
smtOp0 op = smtOpN op []
smtOp1 op x = smtOpN op [x]
smtOp2 op x y = smtOpN op [x, y]

smtBVOpN :: String -> [Int] -> Doc
smtBVOpN op xs = smtOpN "_" (text op : map int xs)
smtBVOp1 op x = smtBVOpN op [x]
smtBVOp2 op x y = smtBVOpN op [x, y]

smtGroup :: [Doc] -> Doc
smtGroup = parens . sep

smtGroup' :: [Doc] -> Doc
smtGroup' [x] = x
smtGroup' xs = smtGroup xs

smtScope :: Writer -> IO a -> IO a
smtScope write body = do
  write SMTCommand $ smtOp0 "push"
  ret <- body
  write SMTCommand $ smtOp0 "pop"
  return ret

smtConfig :: Writer -> IO ()
smtConfig write = do
  write SMTCommand $ smtOp0 "set-logic QF_BV"
  write SMTCommand $ smtOp0 "set-option :parallel.enable true"
  write SMTCommand $ smtOp0 "set-option :parallel.threads.max 4"

smtCheckSat :: Writer -> IO ()
smtCheckSat write = write SMTCommand $ smtOp0 "check-sat"

smtIfSat :: Handle -> IO a -> IO a -> IO a
smtIfSat handle sat unsat = do
  ln <- hGetLine handle
  case ln of
    "sat" -> sat
    "unsat" -> unsat
    _ -> error $ "Unexpected SMT output: '" ++ ln ++ "'"

smtBVType :: Int -> Doc
smtBVType w = smtOp2 "_" (smtText "BitVec") (smtInt w)

smtBool2BV :: Doc -> Doc
smtBool2BV x = smtOpN "ite" [x, smtBVLit 1 1, smtBVLit 1 0]

smtBV2Bool :: Doc -> Doc
smtBV2Bool x = smtOp2 "=" x $ smtBVLit 1 1


-- Netlist helpers --

genName :: NameHints -> String
genName hints
  | Set.null hints = "v"
  | otherwise = intercalate "_" $ filter (not . null) [prefx, root, sufx]
                where nms = Set.toList hints
                      prefxs = [nm | x@(NmPrefix _ nm) <- nms]
                      roots  = [nm | x@(NmRoot   _ nm) <- nms]
                      sufxs  = [nm | x@(NmSuffix _ nm) <- nms]
                      prefx  = intercalate "_" prefxs
                      root   = intercalate "_" roots
                      sufx   = intercalate "_" sufxs

wireName :: Netlist -> WireId -> String
wireName nl (iId, m_outnm) = name ++ richNm ++ outnm ++ "_" ++ show iId
  where outnm = case m_outnm of Just nm -> nm
                                _       -> ""
        net = getNet nl iId
        richNm = case netPrim net of Input      _ nm -> "inpt_" ++ nm
                                     DontCare   _    -> "dontcare"
                                     RegisterEn _ _  -> "reg"
                                     Register   _ _  -> "reg"
                                     _               -> ""
        name = genName $ netNameHints net

fmtWire :: Netlist -> WireId -> Doc
fmtWire netlist wi = text $ wireName netlist wi


-- SMT Definitions --

-- | Define state initial value
defineInit :: (VerifConf, NetConf) -> Netlist -> IO ()
defineInit (VerifConf{..}, NetConf{..}) netlist = do
  forM_ (zip stateFields stateFieldsInit) \((_, field, w), initVals) -> do
    write SMTCommand $ smtOpN "declare-const" [initName field, smtBVType w]
    snd $ foldl (\(pos, io) (len, maybeVal) -> (pos+len, do
        io
        if len == 0 then return () else
          maybe (return ()) (\val ->
            write SMTCommand $ smtOp1 "assert" $ smtOp2 "=" (smtGroup [(smtBVOp2 "extract" (pos+len-1) pos), initName field]) (smtBVLit len val)) maybeVal
      )) (0, return ()) initVals

-- | Define transition function
defineTransition :: (VerifConf, NetConf) -> Netlist -> Net -> IO ()
defineTransition (VerifConf{..}, NetConf{..}) netlist net = do
  write SMTCommand (smtOpN "define-fun" [
      transitionName -- function name
    , smtGroup ( -- arguments
      (map (\(_, field, w) -> smtGroup [stateCurr field, smtBVType w]) stateFields)
      ++ (map (\(_, field, w) -> smtGroup [inputCurr field, smtBVType w]) inputFields)
      ++ [smtGroup [okCurr, (smtText "Bool")]]
      ++ (map (\(_, field, w) -> smtGroup [stateNext field, smtBVType w]) stateFields)
      )
    , smtText "Bool" -- return type
    , body -- function body
    ])
  where
    stateCurr field = text "state_curr_" <> field
    inputCurr field = text "input_curr_" <> field
    okCurr = text "ok_curr"
    stateNext field = text "state_next_" <> field

    body = foldr (\x y -> smtOp2 "let" (smtGroup [x]) y) ret bindings
    ret = smtOpN "and" $ (smtOp2 "=" okCurr okComputed):stateEq

    okComputed = case (netPrim net, netInputs net) of
      (Assert _, [enable, prop]) -> smtOp2 "=>" (smtBV2Bool $ fmtNetInput enable) (smtBV2Bool $ fmtNetInput prop)
      _ -> undefined

    stateEq = map (\(id, field, _) -> smtOp2 "=" (stateNext field) (regInput $ getNet netlist id)) stateFields

    regInput Net{..} = case (netPrim, netInputs) of
      (RegisterEn _ _, [enable, input]) -> smtOpN "ite" [
        smtBV2Bool $ fmtNetInput enable,
        fmtNetInput input,
        stateCurr $ fmtWire netlist (netInstId, Nothing)]
      (Register _ _, [input]) -> fmtNetInput input
      _ -> undefined

    bindings = catMaybes [fmap (\def -> smtGroup [fmtWire netlist (netInstId, Nothing), def]) (fmtPrim netPrim (map fmtNetInput netInputs)) | Net{..} <- map (getNet netlist) $ partialTopologicalSort netlist $ netInstId net]

    fmtPrim :: Prim -> [Doc] -> Maybe Doc
    fmtPrim prim args = case (prim, args) of
      (Const w n, []) -> Just $ smtBVLit w n
      (Identity _, [x]) -> Just $ x
      (ReplicateBit w, [x]) -> Just $ smtGroup [(smtBVOp1 "repeat" w), x]
      (ZeroExtend iw ow, [x]) -> Just $ smtGroup [(smtBVOp1 "zero_extend" (ow-iw)), x]
      (SignExtend iw ow, [x]) -> Just $ smtGroup [(smtBVOp1 "sign_extend" (ow-iw)), x]
      (SelectBits _ hi lo, [x]) -> Just $ smtGroup [(smtBVOp2 "extract" hi lo), x]
      (Not _, [x]) -> Just $ smtOp1 "bvnot" x
      (Add _, [_, _]) -> Just $ smtOpN "bvadd" args
      (Sub _, [_, _]) -> Just $ smtOpN "bvsub" args
      (Mul _ _ _, [_, _]) -> Just $ smtOpN "bvmul" args
      (Div _, [_, _]) -> Just $ smtOpN "bvdiv" args
      (Mod _, [_, _]) -> Just $ smtOpN "bvmod" args
      (And _, [_, _]) -> Just $ smtOpN "bvand" args
      (Or _, [_, _]) -> Just $ smtOpN "bvor" args
      (Xor _, [_, _]) -> Just $ smtOpN "bvxor" args
      (ShiftLeft _ _, [_, _]) -> Just $ smtOpN "bvshl" args
      (ShiftRight _ _, [_, _]) -> Just $ smtOpN "bvshr" args
      (ArithShiftRight _ _, [_, _]) -> Just $ smtOpN "bvashr" args
      (Concat _ _, [_, _]) -> Just $ smtOpN "concat" args
      (LessThan _, [_, _]) -> Just $ smtBool2BV $ smtOpN "bvult" args
      (LessThanEq _, [_, _]) -> Just $ smtBool2BV $ smtOpN "bvule" args
      (Equal _, [_, _]) -> Just $ smtBool2BV $ smtOpN "=" args
      (NotEqual _, [_, _]) -> Just $ smtBool2BV $ smtOpN "distinct" args
      (Mux _ wsel w, sel:xs) -> Just $ mux sel (wsel, 0) xs
      (MergeWrites MStratOr _ w, _) -> Just $ mergeWrites w args
      (Input _ _, _) -> Nothing
      (DontCare w, []) -> Nothing
      (RegisterEn _ _, _) -> Nothing
      (Register _ _, _) -> Nothing
      (Output _ _, _) -> Nothing
      (Assert _, _) -> Nothing
      _ -> error $ "Blarney.Backend.NewSMT: cannot format Prim '" ++ show prim ++ "'"
      where
        mux :: Doc -> (Int, Integer) -> [Doc] -> Doc
        mux _ _ [x] = x
        mux sel (wsel, idx) (x:xs) = smtOpN "ite" [smtOp2 "=" sel $ smtBVLit wsel idx, x, mux sel (wsel, idx+1) xs]

        mergeWrites :: Int -> [Doc] -> Doc
        mergeWrites w [] = smtBVLit w 0
        mergeWrites w (en:val:xs) = smtOp2 "bvor" (smtOpN "ite" [(smtOp2 "=" en $ smtBVLit 1 1), val, smtBVLit w 0]) (mergeWrites w xs)

    fmtNetInput :: NetInput -> Doc
    fmtNetInput (InputWire wi) = case netPrim $ getNet netlist (fst wi) of
      Input _ _ -> inputCurr $ fmtWire netlist wi
      DontCare _ -> inputCurr $ fmtWire netlist wi
      Register _ _ -> stateCurr $ fmtWire netlist wi
      RegisterEn _ _ -> stateCurr $ fmtWire netlist wi
      _ -> fmtWire netlist wi
    fmtNetInput (InputTree prim xs) = fromJust $ fmtPrim prim $ map fmtNetInput xs

assertTransition :: (VerifConf, NetConf) -> (Doc -> Doc) -> (Doc -> Doc) -> Doc -> (Doc -> Doc) -> IO ()
assertTransition (VerifConf{..}, NetConf{..}) stateCurr inputCurr okCurr stateNext =
  write SMTCommand $ smtOp1 "assert" $ smtGroup $
    [transitionName]
    ++ map (\(_, field, _) -> stateCurr field) stateFields
    ++ map (\(_, field, _) -> inputCurr field) inputFields
    ++ [okCurr]
    ++ map (\(_, field, _) -> stateNext field) stateFields

-- | All SMT definitions
defineAll :: (VerifConf, NetConf) -> Netlist -> Net -> IO ()
defineAll conf netlist net = do
  defineInit conf netlist
  defineTransition conf netlist net


-- SMT variables and assertions --

-- | Assert state is distinct from states with lesser indices
assertDistinctState :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
assertDistinctState (VerifConf{..}, NetConf{..}, SeqConf{..}) n =
  forM_ [0..(n-1)] \k ->
    if length stateFields == 0 then return ()
    else write SMTCommand $ smtOp1 "assert" $ smtOpN "or" $ map (\(_, field, _) -> smtOp2 "distinct" (stateName n field) (stateName k field)) stateFields

-- | Add a new state variable (taking into account SeqConf.distinctStates)
addState :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addState conf@(VerifConf{write}, nconf@NetConf{stateFields}, sconf@SeqConf{stateName, distinctStates}) n = do
  forM_ stateFields \(_, field, w) ->
    write SMTCommand $ smtOp2 "declare-const" (stateName n field) (smtBVType w)
  if distinctStates then assertDistinctState conf n else return ()

-- | Add a new input variable
addInput :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addInput (VerifConf{..}, NetConf{..}, SeqConf{..}) n =
  forM_ inputFields \(_, field, w) ->
    write SMTCommand $ smtOp2 "declare-const" (inputName n field) (smtBVType w)

-- | Add a new ok (assertion result) variable
addOk :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addOk (VerifConf{..}, NetConf{..}, SeqConf{..}) n = write SMTCommand $ smtOp2 "declare-const" (okName n) (smtText "Bool")

-- | Setup for bounded verification
addBoundedInit :: (VerifConf, NetConf, SeqConf) -> IO ()
addBoundedInit conf@(VerifConf{..}, NetConf{..}, SeqConf{..}) = do
  addState conf 0
  if length stateFields == 0 then return ()
  else write SMTCommand $ smtOp1 "assert" $ smtOpN "and" $ map (\(_, field, _) -> smtOp2 "=" (stateName 0 field) (initName field)) stateFields

-- | Add 1 depth to bounded verification
addBoundedStep :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addBoundedStep conf@(vconf@VerifConf{..}, nconf@NetConf{..}, SeqConf{..}) depth =
  if depth <= 0 then error "bounded depth must be at least 1" else do
  addInput conf (depth-1)
  addOk conf (depth-1)
  addState conf depth
  assertTransition (vconf, nconf) (stateName (depth-1)) (inputName (depth-1)) (okName (depth-1)) (stateName depth)

-- | Assert bounded property for a given depth
assertBoundedFixed :: (VerifConf, FixedConf, NetConf, SeqConf) -> IO ()
assertBoundedFixed (vconf@VerifConf{write}, FixedConf{depth}, nconf, sconf@SeqConf{okName}) =
  if depth <= 0 then error "bounded depth must be at least 1" else do
  write SMTEcho (smtOp1 "echo" (smtText ("\"(depth " ++ show depth ++ ") Bounded refutation:\"")))
  addBoundedInit conf
  forM_ [1..depth] \k -> addBoundedStep conf k
  write SMTCommand $ smtOp1 "assert" $ smtOp1 "not" $ smtOpN "and" [okName (k-1) | k <- [1..depth]]
  where conf = (vconf, nconf, sconf)

-- | Add partial bounded property for the given depth
assertBoundedIncremental :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
assertBoundedIncremental (VerifConf{..}, NetConf{..}, SeqConf{..}) depth =
  if depth <= 0 then error "bounded depth must be at least 1" else do
  write SMTCommand $ smtOp1 "assert" $ smtOp1 "not" (okName (depth-1))

-- | Setup for induction verification
-- Note: Also serves as combinational verification assertion
-- Note: Called by addAndAssertInductionStep
addInductionInit :: (VerifConf, NetConf, SeqConf) -> IO ()
addInductionInit conf@(vconf@VerifConf{..}, nconf@NetConf{..}, SeqConf{..}) = do
  forM_ stateFields \(_, field, w) ->
    write SMTCommand $ smtOp2 "declare-const" (lastName field) (smtBVType w)
  addInput conf 0
  addState conf 0
  assertTransition (vconf, nconf) (stateName 0) (inputName 0) (smtText "false") lastName

-- | Add 1 depth to induction verification, and assert partial property
addAndAssertInductionStep :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addAndAssertInductionStep conf@(vconf@VerifConf{..}, nconf@NetConf{..}, SeqConf{..}) depth = do
  if depth == 0 then addInductionInit conf else do
    addInput conf depth
    addState conf depth
    assertTransition (vconf, nconf) (stateName depth) (inputName depth) (smtText "true") (stateName (depth-1))

-- | Assert induction property for a given depth
assertInductionFixed :: (VerifConf, FixedConf, NetConf, SeqConf) -> IO ()
assertInductionFixed (vconf@VerifConf{write}, FixedConf{depth}, nconf, sconf) = do
  if depth == 0 then
    write SMTEcho (smtOp1 "echo" (smtText ("\"Combinational refutation:\"")))
  else
    write SMTEcho (smtOp1 "echo" (smtText ("\"(depth " ++ show depth ++ ") Induction refutation:\"")))
  addInductionInit conf
  forM_ [1..depth] \k -> addAndAssertInductionStep conf k
  where conf = (vconf, nconf, sconf)

-- | Assert induction property for a given depth
assertQIFixed :: (VerifConf, FixedConf, NetConf) -> IO ()
assertQIFixed (vconf@VerifConf{write}, FixedConf{depth}, nconf@NetConf{transitionName, inputFields, stateFields}) = do
  if depth == 0 then
    write SMTEcho (smtOp1 "echo" (smtText ("\"Combinational refutation:\"")))
  else
    write SMTEcho (smtOp1 "echo" (smtText ("\"(depth " ++ show depth ++ ") Induction refutation:\"")))
  addInductionInit (vconf, nconf, sconfTail)
  forM_ [1..depth] \k -> addAndAssertInductionStep (vconf, nconf, sconfTail) k
  if depth == 0 then return ()
  else write SMTCommand $ smtOp1 "assert" $ quantify "forall" inputvars $ quantify "exists" statevars $ transitions
  where
    quantify quantifier [] body = body
    quantify quantifier vars body = smtOp2 quantifier (smtGroup vars) body
    sconfTail = mkSeqConf "tail" False
    sconfQuant' = mkSeqConf "quant" False
    sconfQuant = sconfQuant'{stateName=(\d -> if d == depth then stateName sconfTail d else stateName sconfQuant' d)}
    inputvars = concat $ map (\d -> map (\(_, field, w) -> smtGroup [inputName sconfQuant d field, smtBVType w]) inputFields) [1..depth]
    statevars = concat $ map (\d -> map (\(_, field, w) -> smtGroup [stateName sconfQuant d field, smtBVType w]) stateFields) [0..(depth-1)]
    transitions = smtOpN "and" $ map (\d ->
        smtGroup $
          [transitionName]
          ++ map (\(_, field, _) -> stateName sconfQuant d field) stateFields
          ++ map (\(_, field, _) -> inputName sconfQuant d field) inputFields
          ++ [smtText "true"]
          ++ map (\(_, field, _) -> stateName sconfQuant (d-1) field) stateFields
      ) [1..depth]


-- Top level verification helpers --

-- | Apply function to each assert in a circuit
forEachAssert :: Modular a => a -> String -> (Netlist -> Net -> String -> IO ()) -> IO ()
forEachAssert circuit name f =
  onNetlists circuit name \netlists ->
    forM_ (Map.toList netlists) \(name, netlist') ->
      let netlist = runNetlistPass dontCareDeInline netlist' in -- give instance ids to DontCare values
      forM_ (elems netlist) \net ->
        case netPrim net of
          Assert title -> f netlist net title
          _ -> return ()

-- | Helper function for doing one live verification step
verifyLiveStep :: (Verbosity, VerifConf) -> Int -> (IO VerifyResult -> IO VerifyResult) -> (IO VerifyResult -> IO VerifyResult) -> Handle -> IO VerifyResult
verifyLiveStep (verb, VerifConf{write, giveModel}) depth bounded induction handle = do
  boundedRet <- boundedStep
  case boundedRet of
    Insufficient -> inductionStep
    _ -> return boundedRet
  where
    boundedStep =
      if depth == 0 then return Insufficient else
        bounded do
          sayVerboseFlush verb $ "(depth " ++ show depth ++ ") bounded ...... "
          smtIfSat handle
            do
              sayVerboseLn verb $ red "falsifiable"
              if giveModel then getModel handle else return ()
              return Falsifiable
            do
              sayVerboseLn verb $ blue "verified"
              return Insufficient
    inductionStep :: IO VerifyResult
    inductionStep =
      induction do
        if depth == 0 then
          sayVerboseFlush verb $ "combinational .......... "
        else
          sayVerboseFlush verb $ "(depth " ++ show depth ++ ") induction .... "
        smtIfSat handle
          do
            sayVerboseLn verb $ yellow "insufficient"
            return Insufficient
          do
            sayVerboseLn verb $ green "verified"
            return Verified
    getModel handle = do
      write SMTCommand $ smtOp0 "get-model"
      write SMTCommand $ smtOp1 "echo" $ smtText "\"###END###\""
      untilEND
      where
        untilEND = do
          line <- hGetLine handle
          case line of
            "###END###" -> return ()
            _ -> sayVerboseLn verb line >> untilEND

-- | Start SMT session
withSMT :: (Verbosity, VerifConf) -> ((Verbosity, VerifConf) -> Handle -> IO a) -> IO a
withSMT (verb, vconf'@VerifConf{write=write'}) action =
  withCreateProcess smtP \(Just hIn) (Just hOut) _ _ ->
    let write t x =
          case verb of
            Verbose -> write' t x >> write_smt_commands (hPutStrLn hIn . render) t x
            _ -> write_smt_commands (hPutStrLn hIn . render) t x
    in
    let vconf = vconf'{write} in do
    hSetBuffering hIn LineBuffering
    action (verb, vconf) hOut
  where
    render = renderStyle $ Style OneLineMode 0 0
    solverCmd = ("z3", ["-in"])
    smtP = (uncurry proc solverCmd){ std_in  = CreatePipe
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe }

-- | Helper function for live verification
verifyLive :: Modular a => (Verbosity, VerifConf) -> a -> ((Verbosity, VerifConf, NetConf) -> Handle -> Netlist -> Net -> IO VerifyResult) -> IO ()
verifyLive (verb', vconf') circuit verifier = do
  withSMT (verb', vconf') \(verb, vconf@VerifConf{write}) hOut ->
    forEachAssert circuit "#circuit#" \netlist net title ->
      let nconf = mkNetConf netlist net in
      smtScope write do
        sayInfoFlush verb $ "Assertion '" ++ title ++ "': "
        sayVerboseLn verb $ ""
        smtScope write do
          ret <- verifier (verb, vconf, nconf) hOut netlist net
          case ret of
            Verified -> sayInfoLn verb $ green "verified"
            Insufficient -> sayInfoLn verb $ yellow "insufficient"
            Falsifiable -> sayInfoLn verb $ red "falsifiable"


-- Top level verification procedures --

-- Fixed depth strategy: (example at depth 2)
-- defineAll
-- push
--   addBoundedInit
--   addBoundedStep 1
--   addBoundedStep 2
--   assert not (ok_head0 and ok_head1)
--   check-sat
-- pop
-- push
--   addAndAssertInductionStep 0 (=addInductionInit)
--   addAndAssertInductionStep 1
--   addAndAssertInductionStep 2
--   check-sat
-- pop

-- Incremental depth strategy: (example at depth 2)
-- defineAll
-- addBoundedInit
-- addAndAssertInductionStep 0 (=addInductionInit)
-- check-sat
-- addBoundedStep 1
-- push
--   assertBoundedIncremental 1
--   check-sat
-- pop
-- addAndAssertInductionStep 1
-- check-sat
-- addBoundedStep 2
-- push
--   assertBoundedIncremental 2
--   check-sat
-- pop
-- addAndAssertInductionStep 2
-- check-sat

-- | Write SMT script to file for offline, fixed depth, verification
verifyOfflineFixed :: Modular a => (Verbosity, VerifConf, FixedConf) -> a -> String -> String -> IO ()
verifyOfflineFixed (verb, vconf'@VerifConf{write=write'}, fconf@FixedConf{depth}) circuit name dir = do
  sayInfoLn verb $ "Writing SMT script to '" ++ fileName ++ "'..."
  system ("mkdir -p " ++ dir)
  withFile fileName WriteMode \h ->
    let write t x = write' t x >> write_smt (hPutStrLn h . render) t x in
    let vconf = vconf'{write} in do
    smtConfig write
    forEachAssert circuit name \netlist net title -> smtScope write do
      write SMTEcho (smtOp1 "echo" (smtText ("\"Assertion '" ++ title ++ "':\"")))
      let nconf = mkNetConf netlist net in do
        defineAll (vconf, nconf) netlist net
        if depth > 0 then
          smtScope write do
            assertBoundedFixed (vconf, fconf, nconf, boundedConf)
            smtCheckSat write
        else return ()
        smtScope write do
          assertInductionFixed (vconf, fconf, nconf, inductionConf True)
          smtCheckSat write
  where
    fileName = dir ++ "/" ++ name ++ ".smt2"
    render = renderStyle $ Style PageMode 80 1.05

-- | Perform live bounded verification
verifyLiveBounded :: Modular a => (Verbosity, VerifConf, FixedConf) -> a -> IO ()
verifyLiveBounded (verb, vconf, fconf@FixedConf{depth}) circuit =
  verifyLive (verb, vconf) circuit \(verb, vconf@VerifConf{write}, nconf) hOut netlist net -> do
    defineAll (vconf, nconf) netlist net
    verifyLiveStep (verb, vconf) depth
      (\hook -> smtScope write do
        assertBoundedFixed (vconf, fconf, nconf, boundedConf)
        smtCheckSat write
        hook)
      (\hook -> smtScope write do
        write SMTCommand $ smtOp1 "echo" $ smtText "\"sat\"" -- Fake counter example
        hook)
      hOut

verifyOfflineQIFixed :: Modular a => (Verbosity, VerifConf, FixedConf) -> a -> String -> String -> IO ()
verifyOfflineQIFixed (verb, vconf'@VerifConf{write=write'}, fconf@FixedConf{depth}) circuit name dir = do
  sayInfoLn verb $ "Writing SMT script to '" ++ fileName ++ "'..."
  system ("mkdir -p " ++ dir)
  withFile fileName WriteMode \h ->
    let write t x = write' t x >> write_smt (hPutStrLn h . render) t x in
    let vconf = vconf'{write} in do
    --smtConfig write
    forEachAssert circuit name \netlist net title -> smtScope write do
      write SMTEcho (smtOp1 "echo" (smtText ("\"Assertion '" ++ title ++ "':\"")))
      let nconf = mkNetConf netlist net in do
        defineAll (vconf, nconf) netlist net
        if depth > 0 then
          smtScope write do
            assertBoundedFixed (vconf, fconf, nconf, boundedConf)
            smtCheckSat write
        else return ()
        smtScope write do
          assertQIFixed (vconf, fconf, nconf)
          smtCheckSat write
  where
    fileName = dir ++ "/" ++ name ++ ".smt2"
    render = renderStyle $ Style PageMode 80 1.05

-- | Perform live fixed depth verification
verifyLiveFixed :: Modular a => (Verbosity, VerifConf, FixedConf) -> a -> IO ()
verifyLiveFixed (verb, vconf, fconf@FixedConf{depth}) circuit =
  verifyLive (verb, vconf) circuit \(verb, vconf@VerifConf{write}, nconf) hOut netlist net -> do
    defineAll (vconf, nconf) netlist net
    verifyLiveStep (verb, vconf) depth
      (\hook -> smtScope write do
        assertBoundedFixed (vconf, fconf, nconf, boundedConf)
        smtCheckSat write
        hook)
      (\hook -> smtScope write do
        assertInductionFixed (vconf, fconf, nconf, inductionConf True)
        smtCheckSat write
        hook)
      hOut

verifyLiveQIFixed :: Modular a => (Verbosity, VerifConf, FixedConf) -> a -> IO ()
verifyLiveQIFixed (verb, vconf, fconf@FixedConf{depth}) circuit =
  verifyLive (verb, vconf) circuit \(verb, vconf@VerifConf{write}, nconf) hOut netlist net -> do
    defineAll (vconf, nconf) netlist net
    verifyLiveStep (verb, vconf) depth
      (\hook -> smtScope write do
        assertBoundedFixed (vconf, fconf, nconf, boundedConf)
        smtCheckSat write
        hook)
      (\hook -> smtScope write do
        assertQIFixed (vconf, fconf, nconf)
        smtCheckSat write
        hook)
      hOut

-- | Perform live incremental depth verification
verifyLiveIncremental :: Modular a => (Verbosity, VerifConf, IncrementalConf) -> a -> IO ()
verifyLiveIncremental (verb, vconf, iconf) circuit =
  verifyLive (verb, vconf) circuit \(verb, vconf@VerifConf{write}, nconf) hOut netlist net -> do
    defineAll (vconf, nconf) netlist net
    addBoundedInit (vconf, nconf, boundedConf)
    verifier (verb, vconf, iconf, nconf, hOut) 0
  where
    verifier :: (Verbosity, VerifConf, IncrementalConf, NetConf, Handle) -> Int -> IO VerifyResult
    verifier params@(verb, vconf@VerifConf{write}, iconf@IncrementalConf{limit}, nconf, hOut) depth = do
      ret <- verifyLiveStep (verb, vconf) depth
        (\hook -> do
          addBoundedStep (vconf, nconf, boundedConf) depth
          smtScope write do
            assertBoundedIncremental (vconf, nconf, boundedConf) depth
            smtCheckSat write
            hook)
        (\hook -> do
          addAndAssertInductionStep (vconf, nconf, inductionConf True) (depth)
          smtCheckSat write
          hook)
        hOut
      case (ret, fromMaybe True $ fmap (depth <) limit) of
        (Insufficient, True) -> verifier params (depth+1)
        _ -> return ret


-- Concurrent verification --

type CounterEx = (Int, String)

data ProofPart = Bounded Int
               | Induction Int
               | Counter CounterEx
               | Abort

type ProofPartRunner = (Verbosity, VerifConf, NetConf) -> (Netlist, Net) -> Int -> IO (Maybe ProofPart)
type ProofPartGenerator = (MVar ProofPart -> IO ())
type AssertProofPartGenerator = (Verbosity, VerifConf, NetConf) -> (Netlist, Net) -> ProofPartGenerator

data ProofResult = PVerified
                 | PFalsifiable CounterEx
                 | PUnknown

-- | Collect proof parts and stop once a complete proof can be assembled.
proofMerge :: MVar ProofPart -> Int -> Maybe Int -> IO ProofResult
proofMerge m bounded induction =
  if maybe False (bounded >=) induction then return PVerified else do
  x <- takeMVar m
  case x of
    Bounded bounded' -> proofMerge m (max bounded bounded') induction
    Induction induction' -> proofMerge m bounded $ Just (maybe induction' (min induction') induction)
    Counter ex -> return $ PFalsifiable ex
    Abort -> return PUnknown

-- | Run jobs using up to `count` concurrent workers.
-- Works with both finite and infinite jobs lists
runJobs :: (a -> IO ()) -> [a] -> Int -> IO ()
runJobs f jobs count = do
  m <- newEmptyMVar
  withAsync (feed m) $ \src ->
    withAsync (foldr1 concurrently_ $ replicate count (process m)) $ \ret -> do
      waitEither src ret
      cancel src
      wait ret
  where
    feed m = forM_ jobs (putMVar m . Just) >> forever (putMVar m Nothing)
    process m = do
      opt <- takeMVar m
      case opt of
        Just x -> f x >> process m
        Nothing -> return ()

-- | Verify a single assertion
verifyAssert :: ProofPartGenerator -> IO ProofResult
verifyAssert gen = do
  m <- newEmptyMVar
  withAsync (gen m >> putMVar m Abort) $ \src ->
    withAsync (proofMerge m 0 Nothing) $ \ret -> do
      waitEither src ret
      cancel src
      wait ret

-- | Run bounded verification
checkBounded :: ProofPartRunner
checkBounded (verb', vconf', nconf) (netlist, net) depth =
  if depth == 0 then (return $ Just $ Bounded 0) else
  withSMT (verb', vconf') \(verb, vconf@VerifConf{write, giveModel}) handle -> do
    defineAll (vconf, nconf) netlist net
    smtScope write do -- makes z3 faster for some unknown reason
      assertBoundedFixed (vconf, FixedConf{depth}, nconf, boundedConf)
      smtCheckSat write
      smtIfSat handle
        (sayVerboseLn verb ("  Bounded, depth " ++ show depth ++ ": " ++ red "falsifiable") >> (if giveModel then getModel write handle else return "") >>= (\model -> return $ Just $ Counter (depth, model)))
        (sayVerboseLn verb ("  Bounded, depth " ++ show depth ++ ": " ++ blue "verified") >> (return $ Just $ Bounded depth))
  where
    getModel write handle = do
      write SMTCommand $ smtOp0 "get-model"
      write SMTCommand $ smtOp1 "echo" $ smtText "\"###END###\""
      untilEND
      where
        untilEND :: IO String
        untilEND = do
          line <- hGetLine handle
          case line of
            "###END###" -> return ""
            _ -> untilEND >>= \rest -> return (line ++ "\n" ++ rest)

-- | Run restricted states induction verification
checkRestrInd :: ProofPartRunner
checkRestrInd (verb', vconf', nconf) (netlist, net) depth =
  withSMT (verb', vconf') \(verb, vconf@VerifConf{write}) handle -> do
    defineAll (vconf, nconf) netlist net
    smtScope write do -- makes z3 faster for some unknown reason
      assertInductionFixed (vconf, FixedConf{depth}, nconf, inductionConf True)
      smtCheckSat write
      smtIfSat handle
        (sayVerboseLn verb ("  Restr induction, depth " ++ show depth ++ ": " ++ yellow "insufficient") >> (return $ Nothing))
        (sayVerboseLn verb ("  Restr induction, depth " ++ show depth ++ ": " ++ blue "verified") >> (return $ Just $ Induction depth))

-- | Run quantified induction verification
checkQuantInd :: ProofPartRunner
checkQuantInd (verb', vconf', nconf) (netlist, net) depth =
  withSMT (verb', vconf') \(verb, vconf@VerifConf{write}) handle -> do
    defineAll (vconf, nconf) netlist net
    smtScope write do -- makes z3 faster for some unknown reason
      assertQIFixed (vconf, FixedConf{depth}, nconf)
      smtCheckSat write
      smtIfSat handle
        (sayVerboseLn verb ("  Quant induction, depth " ++ show depth ++ ": " ++ yellow "insufficient") >> (return $ Nothing))
        (sayVerboseLn verb ("  Quant induction, depth " ++ show depth ++ ": " ++ blue "verified") >> (return $ Just $ Induction depth))

-- | A stricty increasing sequence.
-- `n` (strictly positive) controls growth rate, the bigger the slower.
-- `curr` is the initial value.
incrSeq :: Int -> Int -> [Int]
incrSeq n curr =
  curr : incrSeq n (((curr * (n+1)) `div` n) + 1)

-- | Parametric proof part generator
-- `depths` is a sequence of depths
-- `count` is the number of concurrent jobs
-- `runner` is the proof part runner
proofPartGenerator :: [Int] -> Int -> ProofPartRunner -> AssertProofPartGenerator
proofPartGenerator depths count runner conf net m =
  runJobs (\depth -> runner conf net depth >>= maybe (return ()) (putMVar m)) depths count

compositeGenerator :: [AssertProofPartGenerator] -> AssertProofPartGenerator
compositeGenerator gens conf net m = foldr1 concurrently_ $ map (\f -> f conf net m) gens

-- | Concurrent verification
verifyCircuit :: Modular a => AssertProofPartGenerator -> (Verbosity, VerifConf) -> a -> IO ()
verifyCircuit gen (verb, vconf@VerifConf{giveModel}) circuit =
  forEachAssert circuit "#circuit#" \netlist net title ->
    let nconf = mkNetConf netlist net in do
    sayInfoFlush verb $ "Assertion '" ++ title ++ "'... "
    sayVerboseLn verb $ ""
    ret <- verifyAssert $ gen (verb, vconf, nconf) (netlist, net)
    sayVerboseFlush verb $ "Assertion '" ++ title ++ "': "
    sayInfoLn verb $
      case ret of
        PVerified -> green "verified"
        PUnknown -> yellow "unknown"
        PFalsifiable (_, model) -> red "falsifiable" ++ if giveModel then "\n" ++ model else ""

-- | Automatic verification procedure
-- Aimed at giving a result as soon as possible
checkAuto :: Modular a => Verbosity -> a -> IO ()
checkAuto verb = verifyCircuit generator (verb, vconfQuiet)
  where generator = compositeGenerator [
                      proofPartGenerator (incrSeq 3 1) 4 checkBounded
                    , proofPartGenerator (incrSeq 9 0) 4 checkQuantInd
                    ]

-- | Fixed depth verification procedure
-- Aimed at giving a result as soon as possible
checkFixed :: Modular a => Int -> Verbosity -> a -> IO ()
checkFixed depth verb = verifyCircuit generator (verb, vconfQuiet)
  where generator = compositeGenerator [
                    proofPartGenerator [depth] 1 checkBounded
                  , proofPartGenerator [depth] 1 checkQuantInd
                  ]

-- | Automatic minimal depth counterexample generation
-- Note: Will run forever if there are no counterexamples
-- TODO: Compare performance with incremental solution
debugAuto :: Modular a => Verbosity -> a -> IO ()
debugAuto verb = verifyCircuit generator (verb, vconfDefault)
  where generator = proofPartGenerator [1..] 1 checkBounded

-- | Fixed depth counterexample generation
-- Note: Returns Unknown if there is no such counterexample
debugFixed :: Modular a => Int -> Verbosity -> a -> IO ()
debugFixed depth verb = verifyCircuit generator (verb, vconfDefault)
  where generator = proofPartGenerator [depth] 1 checkBounded
