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
See last section for verification strategy layout.
-}

module Blarney.Backend.NewSMT (
  Blarney.Backend.NewSMT.Verbosity (..)
, Blarney.Backend.NewSMT.VerifyResult
, Blarney.Backend.NewSMT.OutputType
, Blarney.Backend.NewSMT.Writer
, Blarney.Backend.NewSMT.write_nothing
, Blarney.Backend.NewSMT.write_screen
, Blarney.Backend.NewSMT.VerifConf (..)
, Blarney.Backend.NewSMT.vconfDefault
, Blarney.Backend.NewSMT.vconfDebug
, Blarney.Backend.NewSMT.FixedConf (..)
, Blarney.Backend.NewSMT.fconfCombinational
, Blarney.Backend.NewSMT.IncrementalConf (..)
, Blarney.Backend.NewSMT.iconfDefault
, Blarney.Backend.NewSMT.verifyOfflineFixed
, Blarney.Backend.NewSMT.verifyLiveFixed
, Blarney.Backend.NewSMT.verifyLiveIncremental
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
  restrictedStates :: Bool
, write :: Writer
, giveModel :: Bool
}

vconfDefault = VerifConf { restrictedStates=True, write=write_nothing, giveModel=True }
vconfDebug = VerifConf { restrictedStates=True, write=write_screen, giveModel=True }

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
  stateType :: Doc
, stateConstr :: Doc
, stateFields :: [(InstId, Int)]
, stateInit :: [Doc]
, inputType :: Doc
, inputConstr :: Doc
, inputFields :: [(InstId, Int)]
, initName :: Doc
, lastName :: Doc
, transitionName :: Doc
}

mkNetConf :: Netlist -> NetConf
mkNetConf netlist =
  NetConf {
    stateType = text "State"
  , stateConstr = text "mkState"
  , stateFields = stateFields
  , stateInit = stateInit
  , inputType = text "Input"
  , inputConstr = text "mkInput"
  , inputFields = inputFields
  , initName = text "state_init"
  , lastName = text "state_last"
  , transitionName = text "transition"
  }
  where
    dontCare = error "todo"

    (stateFields, stateInit) = unzip $ catMaybes [
      case netPrim of
        RegisterEn init w -> Just ((netInstId, w), smtBV w $ fromMaybe dontCare init)
        Register init w -> Just ((netInstId, w), smtBV w $ fromMaybe dontCare init)
        _ -> Nothing
      | Net{..} <- elems netlist]
    inputFields = [(netInstId, w) | Net{netPrim=Input w _, netInstId} <- elems netlist]

data SeqConf = SeqConf {
  stateName :: Int -> Doc
, inputName :: Int -> Doc
, okName :: Int -> Doc
, distinctStates :: Bool
}

mkSeqConf :: String -> Bool -> SeqConf
mkSeqConf suffix distinctStates = SeqConf {
  stateName = \n -> text $ "state_" ++ suffix ++ show n
, inputName = \n -> text $ "input_" ++ suffix ++ show n
, okName = \n -> text $ "ok_" ++ suffix ++ show n
, distinctStates = distinctStates
}

boundedConf :: SeqConf
boundedConf = mkSeqConf "head" False

inductionConf :: VerifConf -> SeqConf
inductionConf VerifConf{..} = mkSeqConf "tail" restrictedStates


-- SMT helpers --

smtText :: String -> Doc
smtText = text

smtInt :: Int -> Doc
smtInt = int

smtBV :: Int -> Integer -> Doc
smtBV w n = text $ printf ("#b%0" ++ show w ++ "b") n

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

smtCheckSat :: Writer -> IO ()
smtCheckSat write = write SMTCommand $ smtOp0 "check-sat"

smtBool2BV :: Doc -> Doc
smtBool2BV x = smtOpN "ite" [x, smtBV 1 1, smtBV 1 0]

smtBV2Bool :: Doc -> Doc
smtBV2Bool x = smtOp2 "=" x $ smtBV 1 1

smtDatatype :: Doc -> Doc -> [(Doc, Int)] -> Doc
smtDatatype name constr fields =
  smtOp2 "declare-datatype" name $ smtGroup [
    smtGroup' $ constr : map (\(field, w) -> smtGroup [field, smtOp2 "_" (smtText "BitVec") (smtInt w)]) fields]


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
                                     RegisterEn _ _  -> "reg"
                                     Register   _ _  -> "reg"
                                     _               -> ""
        name = genName $ netNameHints net

fmtWire :: Netlist -> WireId -> Doc
fmtWire netlist wi = text $ wireName netlist wi


-- SMT Definitions --

-- | Define datatypes
defineDatatypes :: (VerifConf, NetConf) -> Netlist -> IO ()
defineDatatypes (VerifConf{..}, NetConf{..}) netlist = do
  write SMTCommand $ smtDatatype stateType stateConstr $ map (\(id, w) -> (fmtWire netlist (id, Nothing), w)) stateFields
  write SMTCommand $ smtDatatype inputType inputConstr $ map (\(id, w) -> (fmtWire netlist (id, Nothing), w)) inputFields

-- | Define state initial value
defineInit :: (VerifConf, NetConf) -> IO ()
defineInit (VerifConf{..}, NetConf{..}) =
  write SMTCommand $ smtOpN "define-const" [initName, stateType, smtGroup' $ stateConstr : stateInit]

-- | Define transition function
defineTransition :: (VerifConf, NetConf) -> Netlist -> Net -> IO ()
defineTransition (VerifConf{..}, NetConf{..}) netlist net = do
  write SMTCommand (smtOpN "define-fun" [
      transitionName -- function name
    , smtGroup [ -- arguments
        smtGroup [stateCurr, stateType]
      , smtGroup [inputCurr, inputType]
      , smtGroup [okCurr, (smtText "Bool")]
      , smtGroup [stateNext, stateType]
      ]
    , smtText "Bool" -- return type
    , body -- function body
    ])
  where
    stateCurr = smtText "state_curr"
    inputCurr = smtText "input_curr"
    okCurr = smtText "ok_curr"
    stateNext = smtText "state_next"

    body = foldr (\x y -> smtOp2 "let" (smtGroup [x]) y) ret bindings
    ret = (smtOp2 "and" (smtOp2 "=" okCurr okComputed) (smtOp2 "=" stateNext stateComputed))

    okComputed = case (netPrim net, netInputs net) of
      (Assert _, [enable, prop]) -> smtOp2 "=>" (smtBV2Bool $ fmtNetInput enable) (smtBV2Bool $ fmtNetInput prop)
      _ -> undefined

    stateComputed = smtGroup' $ stateConstr : map (regInput . getNet netlist . fst) stateFields

    regInput Net{..} = case (netPrim, netInputs) of
      (RegisterEn _ _, [enable, input]) -> smtOpN "ite" [
        smtBV2Bool $ fmtNetInput enable,
        fmtNetInput input,
        smtGroup [fmtWire netlist (netInstId, Nothing), stateCurr]]
      (Register _ _, [input]) -> fmtNetInput input
      _ -> undefined

    bindings = catMaybes [fmap (\def -> smtGroup [text "v_" <> int netInstId, def]) (fmtPrim netPrim (map fmtNetInput netInputs)) | Net{..} <- map (getNet netlist) $ partialTopologicalSort netlist $ netInstId net]

    fmtPrim :: Prim -> [Doc] -> Maybe Doc
    fmtPrim prim args = case (prim, args) of
      (Const w n, []) -> Just $ smtBV w n
      (DontCare w, []) -> Just $ error "todo"
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
      (LessThanEq _, [_, _]) -> Just $ smtOpN "bvule" args
      (Equal _, [_, _]) -> Just $ smtBool2BV $ smtOpN "=" args
      (NotEqual _, [_, _]) -> Just $ smtBool2BV $ smtOpN "distinct" args
      (Mux _ wsel w, sel:xs) -> Just $ mux sel (wsel, 0) xs
      (MergeWrites MStratOr _ w, _) -> Just $ mergeWrites w args
      (Input _ _, _) -> Nothing
      (RegisterEn _ _, _) -> Nothing
      (Register _ _, _) -> Nothing
      (Output _ _, _) -> Nothing
      (Assert _, _) -> Nothing
      _ -> error $ "Blarney.Backend.NewSMT: cannot format Prim '" ++ show prim ++ "'"
      where
        mux :: Doc -> (Int, Integer) -> [Doc] -> Doc
        mux _ _ [x] = x
        mux sel (wsel, idx) (x:xs) = smtOpN "ite" [smtOp2 "=" sel $ smtBV wsel idx, x, mux sel (wsel, idx+1) xs]

        mergeWrites :: Int -> [Doc] -> Doc
        mergeWrites w [] = smtBV w 0
        mergeWrites w (en:val:xs) = smtOp2 "bvor" (smtOpN "ite" [(smtOp2 "=" en $ smtBV 1 1), val, smtBV w 0]) (mergeWrites w xs)

    fmtNetInput :: NetInput -> Doc
    fmtNetInput (InputWire wi) = case netPrim $ getNet netlist (fst wi) of
      Input _ _ -> smtGroup [fmtWire netlist wi, inputCurr]
      Register _ _ -> smtGroup [fmtWire netlist wi, stateCurr]
      RegisterEn _ _ -> smtGroup [fmtWire netlist wi, stateCurr]
      _ -> fmtWire netlist wi
    fmtNetInput (InputTree prim xs) = fromJust $ fmtPrim prim $ map fmtNetInput xs

-- | All SMT definitions
defineAll :: (VerifConf, NetConf) -> Netlist -> Net -> IO ()
defineAll conf netlist net = do
  defineDatatypes conf netlist
  defineInit conf
  defineTransition conf netlist net


-- SMT variables and assertions --

-- | Assert state is distinct from states with lesser indices
assertDistinctState :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
assertDistinctState (VerifConf{..}, NetConf{..}, SeqConf{..}) n =
  forM_ [0..(n-1)] \k -> write SMTCommand $ smtOp1 "assert" $ smtOp2 "distinct" (stateName n) (stateName k)

-- | Add a new state variable (taking into account SeqConf.distinctStates)
addState :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addState conf@(VerifConf{write}, nconf@NetConf{stateType}, sconf@SeqConf{stateName, distinctStates}) n = do
  write SMTCommand $ smtOp2 "declare-const" (stateName n) stateType
  if distinctStates then assertDistinctState conf n else return ()

-- | Add a new input variable
addInput :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addInput (VerifConf{..}, NetConf{..}, SeqConf{..}) n = write SMTCommand $ smtOp2 "declare-const" (inputName n) inputType

-- | Add a new ok (assertion result) variable
addOk :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addOk (VerifConf{..}, NetConf{..}, SeqConf{..}) n = write SMTCommand $ smtOp2 "declare-const" (okName n) (smtText "Bool")

-- | Setup for bounded verification
addBoundedInit :: (VerifConf, NetConf, SeqConf) -> IO ()
addBoundedInit conf@(VerifConf{..}, NetConf{..}, SeqConf{..}) = do
  addState conf 0
  write SMTCommand $ smtOp1 "assert" $ smtOp2 "=" (stateName 0) initName

-- | Add 1 depth to bounded verification
addBoundedStep :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addBoundedStep conf@(VerifConf{..}, NetConf{..}, SeqConf{..}) depth =
  if depth <= 0 then error "bounded depth must be at least 1" else do
  addInput conf (depth-1)
  addOk conf (depth-1)
  addState conf depth
  write SMTCommand $ smtOp1 "assert" $ smtGroup [transitionName, stateName (depth-1), inputName (depth-1), okName (depth-1), stateName depth]

-- | Assert bounded property for a given depth
assertBoundedFixed :: (VerifConf, FixedConf, NetConf, SeqConf) -> IO ()
assertBoundedFixed (vconf@VerifConf{write}, FixedConf{depth}, nconf, sconf@SeqConf{okName}) =
  if depth <= 0 then error "bounded depth must be at least 1" else do
  write SMTEcho (smtOp1 "echo" (smtText ("\"(depth " ++ show depth ++ ") Bounded refutation:\"")))
  addBoundedInit conf
  forM_ [1..depth] \k -> addBoundedStep conf k
  write SMTCommand $ smtOp1 "assert" $ smtOp1 "not" $ foldr1 (smtOp2 "and") [okName (depth-1) | k <- [1..depth]]
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
addInductionInit conf@(VerifConf{..}, NetConf{..}, SeqConf{..}) = do
  write SMTCommand $ smtOp2 "declare-const" lastName stateType
  addInput conf 0
  addState conf 0
  write SMTCommand $ smtOp1 "assert" $ smtGroup [transitionName, stateName 0, inputName 0, smtText "false", lastName]

-- | Add 1 depth to induction verification, and assert partial property
addAndAssertInductionStep :: (VerifConf, NetConf, SeqConf) -> Int -> IO ()
addAndAssertInductionStep conf@(VerifConf{..}, NetConf{..}, SeqConf{..}) depth = do
  if depth == 0 then addInductionInit conf else do
    addInput conf depth
    addState conf depth
    write SMTCommand $ smtOp1 "assert" $ smtGroup [transitionName, stateName depth, inputName depth, smtText "true", stateName (depth-1)]

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


-- Top level verification helpers --

-- | Apply function to each assert in a circuit
forEachAssert :: Modular a => a -> String -> (Netlist -> Net -> String -> IO ()) -> IO ()
forEachAssert circuit name f =
  onNetlists circuit name \netlists ->
    forM_ (Map.toList netlists) \(name, netlist) ->
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
          ifSat
            do
              sayVerboseLn verb $ red "falsifiable"
              if giveModel then getModel else return ()
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
        ifSat
          do
            sayVerboseLn verb $ yellow "insufficient"
            return Insufficient
          do
            sayVerboseLn verb $ green "verified"
            return Verified
    getLine = hGetLine handle
    ifSat sat unsat = do
      ln <- getLine
      case ln of
        "sat" -> sat
        "unsat" -> unsat
        _ -> error $ "Unexpected SMT output: '" ++ ln ++ "'"
    getModel = do
      write SMTCommand $ smtOp1 "get-value" $ smtGroup (map (inputName boundedConf) [0..(depth-1)])
      write SMTCommand $ smtOp1 "echo" $ smtText "\"###END###\""
      untilEND
      where
        untilEND = do
          line <- getLine
          case line of
            "###END###" -> return ()
            _ -> sayVerboseLn verb line >> untilEND

-- | Helper function for live verification
verifyLive :: Modular a => (Verbosity, VerifConf) -> a -> ((Verbosity, VerifConf, NetConf) -> Handle -> Netlist -> Net -> IO VerifyResult) -> IO ()
verifyLive (verb, vconf'@VerifConf{write=write'}) circuit verifier = do
  withCreateProcess smtP \(Just hIn) (Just hOut) _ _ ->
    let write t x =
          case verb of
            Verbose -> write' t x >> write_smt_commands (hPutStrLn hIn . render) t x
            _ -> write_smt_commands (hPutStrLn hIn . render) t x
    in
    let vconf = vconf'{write} in
    do
      hSetBuffering hIn LineBuffering
      forEachAssert circuit "#circuit#" \netlist net title ->
        let nconf = mkNetConf netlist in do
          sayInfoFlush verb $ "Assertion '" ++ title ++ "': "
          sayVerboseLn verb $ ""
          smtScope write do
            ret <- verifier (verb, vconf, nconf) hOut netlist net
            case ret of
              Verified -> sayInfoLn verb $ green "verified"
              Insufficient -> sayInfoLn verb $ yellow "insufficient"
              Falsifiable -> sayInfoLn verb $ red "falsifiable"
  where
    render = renderStyle $ Style PageMode 80 1.05
    solverCmd = ("z3", ["-in"])
    smtP = (uncurry proc solverCmd){ std_in  = CreatePipe
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe }


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
    let vconf = vconf'{write} in
    forEachAssert circuit name \netlist net title -> do
      write SMTEcho (smtOp1 "echo" (smtText ("\"Assertion '" ++ title ++ "':\"")))
      let nconf = mkNetConf netlist in do
        defineAll (vconf, nconf) netlist net
        if depth > 0 then
          smtScope write do
            assertBoundedFixed (vconf, fconf, nconf, boundedConf)
            smtCheckSat write
        else return ()
        smtScope write do
          assertInductionFixed (vconf, fconf, nconf, inductionConf vconf)
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
        assertInductionFixed (vconf, fconf, nconf, inductionConf vconf)
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
          addAndAssertInductionStep (vconf, nconf, inductionConf vconf) (depth)
          smtCheckSat write
          hook)
        hOut
      case (ret, fromMaybe True $ fmap (depth <) limit) of
        (Insufficient, True) -> verifier params (depth+1)
        _ -> return ret
